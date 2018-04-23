object SbtReleaseProcess {

  import sbt._
  import sbtrelease.Git
  import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
  import sbtrelease.ReleasePlugin.autoImport._
  import sbtrelease.Utilities._

  import sys.process.ProcessLogger


  private def getGIT(st: State): Git = st.extract.get(releaseVcs).get.asInstanceOf[Git]

  private def toProcessLogger(st: State): ProcessLogger = new ProcessLogger {
    override def err(s: => String): Unit = st.log.info(s)
    override def out(s: => String): Unit = st.log.info(s)
    override def buffer[T](f: => T): T = st.log.buffer(f)
  }

  /**
    * Merge the `devBranch` into the `deployBranch`.
    * Usually, merge the develop branch into master
    */
  private def mergeReleaseVersion(devBranch: String, deployBranch: String): (State) => State = { st: State =>
    val logger = toProcessLogger(st)
    val git = getGIT(st)

    st.log.info(s"Merging $devBranch in $deployBranch")
    git.cmd("checkout", deployBranch) ! logger
    git.cmd("pull --ff-only") ! logger
    git.cmd("merge", devBranch, "--no-edit") ! logger
    git.cmd("push", "origin", s"$deployBranch:$deployBranch") ! logger
    git.cmd("checkout", devBranch) ! logger
    st.log.info("Merge complete with success")

    st
  }

  /**
    * Check if the repository is currently on the required branch
    */
  private def checkCurrentBranch(requiredBranch: String): (State) => State = { st: State =>
    val git = getGIT(st)
    val currentBranch = git.currentBranch
    if (currentBranch != requiredBranch) sys.error(s"You must be on the $requiredBranch branch to release a version. Actually you're on the $currentBranch")
    st
  }

  val steps = Seq[ReleaseStep](
    checkCurrentBranch("develop"),
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    mergeReleaseVersion("develop", "master"),
    tagRelease,
    setNextVersion,
    commitNextVersion,
    pushChanges
  )

}

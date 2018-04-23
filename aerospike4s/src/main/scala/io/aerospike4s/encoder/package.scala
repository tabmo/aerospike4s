package io.aerospike4s

package object encoder {

  def field[A](path: String)(implicit next: Encoder[A]): Encoder[A] = new Encoder[A] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[A] = ev.field(path)(next)
  }
}

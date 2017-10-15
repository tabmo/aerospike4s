package io.aerospike4s

package object decoder {

  def field[A](path: String)(implicit next: Decoder[A]): Decoder[A] = new Decoder[A] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.field(path)(next)
  }

  def at[A](idx: Int)(implicit next: Decoder[A]): Decoder[A] = new Decoder[A] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.at(idx)(next)
  }
}

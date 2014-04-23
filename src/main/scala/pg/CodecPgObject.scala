package com.benkolera.slick.pg

trait CodecPgObject[A] extends DecodePgObject[A] with EncodePgObject[A] {
  val typeName: String
  def stringEncode(a:A):String
  def stringDecode(s:String): A
}

object CodecPgObject extends EncodePgObjects with DecodePgObjects {
  def apply[A](n:String)(e:A => String, d:String => A): CodecPgObject[A] =
    new CodecPgObject[A]{
      def stringDecode(s:String) = d(s)
      def stringEncode(a:A) = e(a)
      val typeName = n
    }
}

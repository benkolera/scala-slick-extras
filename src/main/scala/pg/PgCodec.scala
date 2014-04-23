package com.benkolera.slick.pg

trait PgCodec extends EncodePgObjects with DecodePgObjects {

  def pgCodec[A] = CodecPgObject[A] _
  def pgEncode[A] = EncodePgObject[A] _
  def pgDecode[A] = DecodePgObject[A] _

}

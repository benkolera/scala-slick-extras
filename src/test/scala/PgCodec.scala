package com.benkolera.slick.pg

import org.specs2.mutable._
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

class PgCodecSpec extends Specification with PgCodec{

  case class Test(i:Int)
  implicit val intCodec = pgCodec[Test]( "int4" )(
    _.i.toString, s => Test(s.toInt)
  )

  "Codec" should {
    "decode" in {
      val query = sql"""SELECT 1""".as[Test]
      true must_==( true )
    }
    "encode" in {
      val i = Test(5)
      val query = sql"""SELECT $i == 5""".as[Boolean]
      true must_==( true )
    }
    "decode option" in {
      val query = sql"""SELECT 1""".as[Option[Test]]
      true must_==( true )
    }
    "encode option" in {
      val i = Some(Test(5))
      val query = sql"""SELECT $i == 5""".as[Boolean]
      true must_==( true )
    }
    "decode list" in {
      val query = sql"""SELECT '{1,2,3,4}'::_int4""".as[List[Test]]
      true must_==( true )
    }
    "encode list" in {
      val i = List(1,2,3,4).map( Test )
      val query = sql"""SELECT 5 = ANY($i)""".as[Boolean]
      true must_==( true )
    }
  }

}

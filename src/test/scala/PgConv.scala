package com.benkolera.slick.pg

import org.specs2.mutable._
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

class ImplicitsSpec extends Specification {

  object PgInt extends PgOptListConv[Int]{
    val pgType = "int4"
    def fromSql( s:String ) = s.toInt
    def toSql( i:Int ) = i.toString
  }
  import PgInt._

  "PgConv" should {
    "get base type" in {
      val query = sql"""SELECT 1""".as[Int]
      true must_==( true )
    }
    "set base type" in {
      val i = 5
      val query = sql"""SELECT $i == 5""".as[Boolean]
      true must_==( true )
    }
    "get option type" in {
      val query = sql"""SELECT 1""".as[Option[Int]]
      true must_==( true )
    }
    "set option type" in {
      val i = Some(5)
      val query = sql"""SELECT $i == 5""".as[Boolean]
      true must_==( true )
    }
    "get list type" in {
      val query = sql"""SELECT '{1,2,3,4}'::integer[]""".as[List[Int]]
      true must_==( true )
    }
    "set list type" in {
      val i = List(1,2,3,4)
      val query = sql"""SELECT 5 = ANY($i)""".as[Boolean]
      true must_==( true )
    }
    "get option list type" in {
      val query = sql"""SELECT '{1,2,3,4}'::integer[]""".as[Option[List[Int]]]
      true must_==( true )
    }
    "set option list type" in {
      val i = None
      val query = sql"""SELECT 5 = ANY($i)""".as[Boolean]
      true must_==( true )
    }
  }

}

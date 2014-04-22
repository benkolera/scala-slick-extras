package com.benkolera.slick.pg

import scala.slick.jdbc.{
  PositionedParameters,
  PositionedResult,
  GetResult,
  SetParameter}
import java.sql.Types.OTHER

trait PgConv[A] {
  val pgType:String
  def fromSql( s:String ): A
  def toSql( a:A ): String

  implicit def getPgResult = new GetResult[A] {
    def apply( pr:PositionedResult ) = fromSql( pr.nextString )
  }

  implicit def setPgParameter = new SetParameter[A] { self =>
    def apply( a:A, pp:PositionedParameters ) {
      pp.setObject( PgObject( pgType, toSql(a) ), OTHER )
    }
  }
}

trait PgOptConv[A] extends PgConv[A] {
  implicit def getPgOptionResult = new GetResult[Option[A]] {
    def apply( pr:PositionedResult ) =
      pr.nextStringOption.map( fromSql )
  }

  implicit def setPgOptionParameter = new SetParameter[Option[A]] { self =>
    def apply( a:Option[A], pp:PositionedParameters ) {
      pp.setObjectOption( a.map( s => PgObject( pgType, toSql(s) ) ), OTHER )
    }
  }
}

trait PgListConv[A] extends PgConv[A] {
  def toSqlArray( l:List[A] ) = "{" + l.map( toSql ).mkString(",") + "}"
  def fromSqlArray( s:String ) = s.tail.init.split(",").toList match{
    case ""::Nil => Nil
    case x       => x.map( fromSql )
  }

  implicit def getPgListResult = new GetResult[List[A]] {
    def apply( pr:PositionedResult ) = fromSqlArray( pr.nextString )
  }

  implicit def setPgListParameter = new SetParameter[List[A]] { self =>
    def apply( a:List[A], pp:PositionedParameters ) {
      pp.setObject( PgObject( "_" + pgType,  toSqlArray( a ) ), OTHER )
    }
  }
}

trait PgOptListConv[A] extends PgListConv[A] with PgOptConv[A] {
  implicit def getPgOptionListResult = new GetResult[Option[List[A]]] {
    def apply( pr:PositionedResult ) =
      pr.nextStringOption.map( fromSqlArray )
  }

  implicit def setPgOptionListParameter = new SetParameter[Option[List[A]]] { self =>
    def apply( a:Option[List[A]], pp:PositionedParameters ) {
      pp.setObjectOption(
        a.map( s => PgObject( "_" + pgType, toSqlArray( s ) ) ), OTHER
      )
    }
  }
}

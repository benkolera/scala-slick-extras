package com.benkolera.slick.joda
package pg

import org.joda.time.{DateTime,DateTimeZone}
import java.sql.Timestamp
import org.postgresql.PGStatement

object PgDateTime {
  def apply( ldt:DateTime ) = Defined(ldt)
  val posInf = PosInfinity[DateTime]()
  val negInf = NegInfinity[DateTime]()

  def fromSql( sql:java.sql.Timestamp ) = {
    foldMillis( sql.getTime )(
      negInfinity = NegInfinity[DateTime](),
      posInfinity = PosInfinity[DateTime](),
      defined     = (t => Defined( new DateTime(t) ))
    )
  }
  def toSql( pgdt:PgDateTime ) = {
    new java.sql.Timestamp(
      getMillisInfinite( pgdt.map( _.withZone(DateTimeZone.getDefault) ) )
    )
  }
}

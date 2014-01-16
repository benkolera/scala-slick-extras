package com.benkolera.slick.joda
package pg

import org.joda.time.{DateTime,DateTimeZone}
import java.sql.Timestamp
import org.postgresql.PGStatement

trait PgDateTimeTypes {
  type PgDateTime = InfiniteInstant[DateTime]
  implicit val pgDateTimeInstant = new Instant[DateTime]{
    def isBefore(a:DateTime,b:DateTime) = a.isBefore(b)
    def isAfter(a:DateTime,b:DateTime)  = a.isAfter(b)
    def isEqual(a:DateTime,b:DateTime)  = a.isEqual(b)
    def getMillis(a:DateTime)  = a.toDateTime.getMillis
  }
}

object PgDateTime {
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

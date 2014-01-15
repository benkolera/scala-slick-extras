package com.benkolera.slick.joda
package pg

import org.joda.time.DateTime
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
    sql.getTime match {
      case PGStatement.DATE_POSITIVE_INFINITY => PosInfinity[DateTime]()
      case PGStatement.DATE_NEGATIVE_INFINITY => NegInfinity[DateTime]()
      case t => Defined( new DateTime(t) )
    }
  }
  def toSql( pgldt:PgDateTime ) = {
    new java.sql.Timestamp(
      pgldt match {
        case PosInfinity() => PGStatement.DATE_POSITIVE_INFINITY
        case NegInfinity() => PGStatement.DATE_NEGATIVE_INFINITY
        case Defined(dt)   => dt.getMillis
      }
    )
  }
}

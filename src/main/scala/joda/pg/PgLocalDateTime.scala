package com.benkolera.slick.joda
package pg

import org.joda.time.LocalDateTime
import java.sql.Timestamp
import org.postgresql.PGStatement

trait PgLocalDateTimeTypes {
  type PgLocalDateTime = InfiniteInstant[LocalDateTime]
  implicit val pgLocalDateTimeInstant = new Instant[LocalDateTime]{
    def isBefore(a:LocalDateTime,b:LocalDateTime) = a.isBefore(b)
    def isAfter(a:LocalDateTime,b:LocalDateTime)  = a.isAfter(b)
    def isEqual(a:LocalDateTime,b:LocalDateTime)  = a.isEqual(b)
    def getMillis(a:LocalDateTime)  = a.toDateTime.getMillis
  }
}

object PgLocalDateTime {
  def fromSql( sql:java.sql.Timestamp ) = {
    sql.getTime match {
      case PGStatement.DATE_POSITIVE_INFINITY => PosInfinity[LocalDateTime]()
      case PGStatement.DATE_NEGATIVE_INFINITY => NegInfinity[LocalDateTime]()
      case t => Defined( new LocalDateTime(t) )
    }
  }
  def toSql( pgldt:PgLocalDateTime ) = {
    new java.sql.Timestamp(
      pgldt match {
        case PosInfinity() => PGStatement.DATE_POSITIVE_INFINITY
        case NegInfinity() => PGStatement.DATE_NEGATIVE_INFINITY
        case Defined(dt)   => dt.toDateTime.getMillis
      }
    )
  }
}

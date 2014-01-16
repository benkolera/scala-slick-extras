package com.benkolera.slick.joda
package pg

import org.joda.time.LocalDateTime
import java.sql.Timestamp
import org.postgresql.PGStatement

object PgLocalDateTime {
  type PgLocalDateTime = InfiniteInstant[LocalDateTime]
  type PgLocalDateTimeRange = InstantRange[LocalDateTime]
  implicit val pgLocalDateTimeInstant = new Instant[LocalDateTime]{
    def isBefore(a:LocalDateTime,b:LocalDateTime) = a.isBefore(b)
    def isAfter(a:LocalDateTime,b:LocalDateTime)  = a.isAfter(b)
    def isEqual(a:LocalDateTime,b:LocalDateTime)  = a.isEqual(b)
    def getMillis(a:LocalDateTime)  = a.toDateTime.getMillis
  }

  def fromSql( sql:java.sql.Timestamp ): InfiniteInstant[LocalDateTime] = {
    foldMillis( sql.getTime )(
      negInfinity = NegInfinity[LocalDateTime](),
      posInfinity = PosInfinity[LocalDateTime](),
      defined     = (t => Defined( new LocalDateTime(t) ))
    )
  }
  def toSql( pgldt:PgLocalDateTime ) = {
    new java.sql.Timestamp(getMillisInfinite( pgldt ))
  }

  def rangeFromSql = InstantRange.fromSql( fromSql ) _
  def rangeToSql = InstantRange.toSql( toSql ) _
}

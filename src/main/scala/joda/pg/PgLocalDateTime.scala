package com.benkolera.slick.joda
package pg

import org.joda.time.LocalDateTime
import java.sql.Timestamp
import org.postgresql.PGStatement

object PgLocalDateTime {
  def apply( ldt:LocalDateTime ) = Defined(ldt)
  val posInf = PosInfinity[LocalDateTime]()
  val negInf = NegInfinity[LocalDateTime]()

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

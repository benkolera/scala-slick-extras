package com.benkolera.slick.joda

import org.postgresql.PGStatement
import org.joda.time.{LocalDateTime,DateTime}

package object pg {
  def foldMillis[A]( ms:Long )(
    negInfinity: => A,
    posInfinity: => A,
    defined: Long => A
  ) = {
    ms match {
      case PGStatement.DATE_POSITIVE_INFINITY => posInfinity
      case PGStatement.DATE_NEGATIVE_INFINITY => negInfinity
      case t => defined(t)
    }
  }

  def getMillisInfinite[A]( ii:InfiniteInstant[A] )( implicit i:Instant[A] ) =
    InfiniteInstant.fold(ii)(
      negInfinity = PGStatement.DATE_NEGATIVE_INFINITY,
      posInfinity = PGStatement.DATE_POSITIVE_INFINITY,
      defined     = i.getMillis _
    )

  type PgLocalDateTime = InfiniteInstant[LocalDateTime]
  type PgLocalDateTimeRange = InstantRange[LocalDateTime]
  implicit val pgLocalDateTimeInstant = new Instant[LocalDateTime]{
    def isBefore(a:LocalDateTime,b:LocalDateTime) = a.isBefore(b)
    def isAfter(a:LocalDateTime,b:LocalDateTime)  = a.isAfter(b)
    def isEqual(a:LocalDateTime,b:LocalDateTime)  = a.isEqual(b)
    def getMillis(a:LocalDateTime)  = a.toDateTime.getMillis
  }

  type PgDateTime = InfiniteInstant[DateTime]
  implicit val pgDateTimeInstant = new Instant[DateTime]{
    def isBefore(a:DateTime,b:DateTime) = a.isBefore(b)
    def isAfter(a:DateTime,b:DateTime)  = a.isAfter(b)
    def isEqual(a:DateTime,b:DateTime)  = a.isEqual(b)
    def getMillis(a:DateTime)  = a.toDateTime.getMillis
  }

}

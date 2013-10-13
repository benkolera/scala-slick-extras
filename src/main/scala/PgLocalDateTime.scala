package com.benkolera.slick

import java.sql.Timestamp
import org.joda.time.LocalDateTime
import org.postgresql.PGStatement

/*
 * Just whipped this up after realising that joda time can't even handle a
 * date big enough for import org.postgresql.PGStatement.POSITIVE_INFINITY.
 *
 * This makes this code tied to postgres, but mysql doesn't really have an
 * infinity anyhow.
 */

sealed trait PgLocalDateTime {
  def isPositiveInfinity:Boolean
  def isNegativeInfinity:Boolean
  def isBefore( otherPg: PgLocalDateTime ):Boolean
  def isAfter( otherPg: PgLocalDateTime ):Boolean
  def isEqual( otherPg: PgLocalDateTime ):Boolean

  def map( f:LocalDateTime => LocalDateTime ):PgLocalDateTime

  def isBeforeOrEqual( otherPg: PgLocalDateTime ):Boolean = {
    isEqual(otherPg) || isBefore(otherPg)
  }
  def isAfterOrEqual( otherPg: PgLocalDateTime ):Boolean = {
    isEqual(otherPg) || isAfter(otherPg)
  }
}

object PgLocalDateTime {
  def fromSql( sql:java.sql.Timestamp ) = {
    sql.getTime match {
      case PGStatement.DATE_POSITIVE_INFINITY => PosInfinity
      case PGStatement.DATE_NEGATIVE_INFINITY => NegInfinity
      case t => Defined( new LocalDateTime(t) )
    }
  }
  def toSql( pgldt:PgLocalDateTime ) = {
    new java.sql.Timestamp(
      pgldt match {
        case PosInfinity => PGStatement.DATE_POSITIVE_INFINITY
        case NegInfinity => PGStatement.DATE_NEGATIVE_INFINITY
        case Defined(dt) => dt.toDateTime.getMillis
      }
    )
  }
}

object NegInfinity extends PgLocalDateTime {
  val isPositiveInfinity = false
  val isNegativeInfinity = true
  def isBefore( otherPg: PgLocalDateTime ) = otherPg match {
    case NegInfinity => false
    case _ => true
  }
  def isAfter( otherPg: PgLocalDateTime ) = false
  def isEqual( otherPg: PgLocalDateTime ) = otherPg match {
    case NegInfinity => true
    case _ => false
  }
  def map( f:LocalDateTime => LocalDateTime):PgLocalDateTime = this
}
object PosInfinity extends PgLocalDateTime {
  val isPositiveInfinity = true
  val isNegativeInfinity = false
  def isBefore( otherPg: PgLocalDateTime ) = false
  def isAfter( otherPg: PgLocalDateTime ) = otherPg match {
    case PosInfinity => false
    case _           => true
  }
  def isEqual( otherPg: PgLocalDateTime ) = otherPg match {
    case PosInfinity => true
    case _ => false
  }
  def map( f:LocalDateTime => LocalDateTime):PgLocalDateTime = this
}
case class Defined( dt:LocalDateTime ) extends PgLocalDateTime {
  val isPositiveInfinity = false
  val isNegativeInfinity = false
  def isBefore( otherPg: PgLocalDateTime ) = otherPg match {
    case PosInfinity  => true
    case NegInfinity  => false
    case Defined(dt2) => dt.isBefore(dt2)
  }
  def isAfter( otherPg: PgLocalDateTime ) = otherPg match {
    case PosInfinity  => false
    case NegInfinity  => true
    case Defined(dt2) => dt.isAfter(dt2)
  }
  def isEqual( otherPg: PgLocalDateTime ) = otherPg match {
    case Defined(dt2) => dt.isEqual(dt2)
    case _ => false
  }
  def map( f:LocalDateTime => LocalDateTime ) = Defined( f(dt) )
}

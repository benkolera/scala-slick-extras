package com.benkolera.slick.joda

import java.sql.Timestamp

sealed trait InstantRange[A] {
  def contains( pgdr:InstantRangeBound[A] )( implicit ci:Instant[A] ): Boolean
  def contains( pgdr:InstantRange[A] )( implicit ci:Instant[A] ): Boolean
  def overlaps( pgdr:InstantRange[A] )( implicit ci:Instant[A] ): Boolean

  def contains( pgdt:InfiniteInstant[A] )( implicit ci:Instant[A] ): Boolean =
    contains( InclusiveStart(pgdt) )
}

object InstantRange {
  def apply[A](
    start:InstantRangeStart[A], end:InstantRangeEnd[A]
  )( implicit ci:Instant[A] ): InstantRange[A] =
    if ( start.date.isAfter(end.date) )
      throw new Exception( "InstantRange lower bound must be less than or equal to upper bound" )
    else if ( start.date.isEqual(end.date) && !(start.isInclusive && end.isInclusive) )
      EmptyRange()
    else
      DefinedRange( start, end )

  def fromSql[A]( f:Timestamp => InfiniteInstant[A] )( sql:String )( implicit ci:Instant[A] ): InstantRange[A] =
    InstantRangeParser.fromSql( f )( sql )
  def toSql[A]( f:InfiniteInstant[A] => Timestamp )( inst:InstantRange[A] ): String =
    inst match {
      case EmptyRange() => "empty"
      case DefinedRange( start, end ) => start.toSql(f) ++ "," ++ end.toSql(f)
    }
}

case class EmptyRange[A]() extends InstantRange[A] {
  def contains( pgdr:InstantRangeBound[A] )( implicit ci:Instant[A] ) = false
  def contains( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) = false
  def overlaps( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) = false
}

case class DefinedRange[A](
  startDate: InstantRangeStart[A]
  , endDate: InstantRangeEnd[A]
) extends InstantRange[A] {
  def contains( pgdr:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    startDate.isBefore( pgdr ) && endDate.isAfter( pgdr )

  def contains( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) =
    pgdr match {
      case EmptyRange() => false
      case DefinedRange(start, end) => contains( start ) && contains( end )
    }

  def overlaps( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) =
    pgdr match {
      case EmptyRange() => false
      case DefinedRange(start, end) => contains( start ) || contains( end )
    }
}

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
    InstantRangeParser[A](f).fromSql( sql )
  def toSql[A]( f:InfiniteInstant[A] => Timestamp )( inst:InstantRange[A] ): String =
    inst match {
      case EmptyRange() => "empty"
      case DefinedRange( start, end ) => start.toSql(f) ++ "," ++ end.toSql(f)
      case ValidityRange( start, end ) =>
        start.toSql(f) ++ "," ++ end.toSql(f)
    }
}

case class EmptyRange[A]() extends InstantRange[A] {
  def contains( pgdr:InstantRangeBound[A] )( implicit ci:Instant[A] ) = false
  def contains( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) = false
  def overlaps( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) = false
}

sealed trait FullRange[A] extends InstantRange[A] {
  val startBound: InstantRangeStart[A]
  val endBound: InstantRangeEnd[A]

  def contains( pgdr:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    startBound.isBefore( pgdr ) && endBound.isAfter( pgdr )

  def contains( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) =
    pgdr match {
      case EmptyRange() => false
      case DefinedRange(start, end) => contains( start ) && contains( end )
      case ValidityRange(start, end) =>
        contains( start ) && contains( end )
    }

  def overlaps( pgdr:InstantRange[A] )( implicit ci:Instant[A] ) =
    pgdr match {
      case EmptyRange() => false
      case DefinedRange(start, end) => contains( start ) || contains( end )
      case ValidityRange(start, end) =>
        contains( start ) || contains( end )
    }
}

case class DefinedRange[A](
  startBound: InstantRangeStart[A]
  , endBound: InstantRangeEnd[A]
) extends FullRange[A]

object ValidityRange {
  def safeApply[A:Instant](
    start:InfiniteInstant[A], end:InfiniteInstant[A]
  ): Option[ValidityRange[A]] =
    if ( start.isAfterOrEqual(end) ) None
    else Some( new ValidityRange( InclusiveStart(start), ExclusiveEnd(end) ) )

  def apply[A:Instant](
    start:InfiniteInstant[A], end:InfiniteInstant[A]
  ): ValidityRange[A] =
    safeApply[A](start,end) match {
      case Some(x) => x
      case None    => throw new Exception(
        "Lower bound must be less than upper bound in a ValidityRange"
      )
    }

  def apply[A:Instant]( start:A, end:A ): ValidityRange[A] =
    apply( Defined(start), Defined(end) )

  def fromSql[A:Instant]( f:Timestamp => InfiniteInstant[A] )( sql:String ): ValidityRange[A] =
    InstantRangeParser[A](f).validityFromSql( sql )
  def toSql[A]( f:InfiniteInstant[A] => Timestamp )( inst:ValidityRange[A] ): String =
    inst.startBound.toSql(f) ++ "," ++ inst.endBound.toSql(f)
}

case class ValidityRange[A](
  startBound: InclusiveStart[A]
  , endBound: ExclusiveEnd[A]
) extends FullRange[A] {
  val startDate = startBound.date
  val endDate = endBound.date
}

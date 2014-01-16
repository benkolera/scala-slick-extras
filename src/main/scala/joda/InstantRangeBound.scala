package com.benkolera.slick.joda

import java.sql.Timestamp

sealed trait InstantRangeBound[A] {
  val date: InfiniteInstant[A]
  val isInclusive: Boolean
  def toSql(f:InfiniteInstant[A] => Timestamp): String
  def instant2String(f:InfiniteInstant[A] => Timestamp) =
    date match {
      case NegInfinity() => "-infinity"
      case PosInfinity() => "infinity"
      case Defined(dt) => "\"" ++ f(date).toString ++ "\""
    }
}

sealed trait InstantRangeStart[A] extends InstantRangeBound[A] {
  val startChar: String
  def isBefore( other:InstantRangeBound[A] )( implicit ci:Instant[A] ): Boolean
  def toSql(f:InfiniteInstant[A] => Timestamp) = startChar ++ instant2String(f)
}

sealed trait InstantRangeEnd[A] extends InstantRangeBound[A] {
  val endChar: String
  def isAfter( other:InstantRangeBound[A] )( implicit ci:Instant[A] ): Boolean
  def toSql(f:InfiniteInstant[A] => Timestamp) = instant2String(f) ++ endChar
}

sealed trait Inclusive {
  val startChar = "["
  val endChar = "]"
  val isInclusive = true
}

sealed trait Exclusive {
  val startChar = "("
  val endChar = ")"
  val isInclusive = false
}

case class InclusiveStart[A]( date:InfiniteInstant[A] ) extends InstantRangeStart[A] with Inclusive {
  def isBefore( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    other match {
      case ExclusiveEnd(dt)   => date.isBefore(dt)
      case _ => date.isBeforeOrEqual(other.date)
    }
}

case class ExclusiveStart[A]( date:InfiniteInstant[A] ) extends InstantRangeStart[A] with Exclusive {
  def isBefore( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    other match {
      case ExclusiveStart(dt) => date.isBeforeOrEqual(dt)
      case _ => date.isBefore(other.date)
    }
}

case class EmptyStart[A]() extends InstantRangeStart[A] with Exclusive {
  val date = NegInfinity[A]()
  val start = ExclusiveStart[A](date)
  def isBefore( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) = start.isBefore( other )
  override def toSql(f:InfiniteInstant[A] => Timestamp) = startChar
}

case class InclusiveEnd[A]( date:InfiniteInstant[A] ) extends InstantRangeEnd[A] with Inclusive {
  def isAfter( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    other match {
      case ExclusiveStart(dt) => date.isAfter(dt)
      case _ => date.isAfterOrEqual(other.date)
    }
}

case class ExclusiveEnd[A]( date:InfiniteInstant[A] ) extends InstantRangeEnd[A] with Exclusive {
  def isAfter( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) =
    other match {
      case ExclusiveEnd(dt)   => date.isAfterOrEqual(dt)
      case _ => date.isAfter(other.date)
    }
}

case class EmptyEnd[A]() extends InstantRangeEnd[A] with Exclusive {
  val date = PosInfinity[A]()
  val end = ExclusiveEnd[A](date)
  def isAfter( other:InstantRangeBound[A] )( implicit ci:Instant[A] ) = end.isAfter( other )
  override def toSql(f:InfiniteInstant[A] => Timestamp) = endChar
}

package com.benkolera.slick.joda

sealed trait InfiniteInstant[A] {
  def isPositiveInfinity:Boolean
  def isNegativeInfinity:Boolean
  def isBefore( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ):Boolean
  def isAfter( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ):Boolean
  def isEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ):Boolean

  def map( f:A => A ):InfiniteInstant[A]

  def isBeforeOrEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ):Boolean = {
    isEqual(otherPg) || isBefore(otherPg)
  }
  def isAfterOrEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ):Boolean = {
    isEqual(otherPg) || isAfter(otherPg)
  }
}

case class NegInfinity[A]() extends InfiniteInstant[A] {
  val isPositiveInfinity = false
  val isNegativeInfinity = true
  def isBefore( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case NegInfinity() => false
    case _ => true
  }
  def isAfter( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = false
  def isEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case NegInfinity() => true
    case _ => false
  }
  def map( f:A => A) = this
}

case class PosInfinity[A]() extends InfiniteInstant[A] {
  val isPositiveInfinity = true
  val isNegativeInfinity = false
  def isBefore( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = false
  def isAfter( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case PosInfinity() => false
    case _           => true
  }
  def isEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case PosInfinity() => true
    case _ => false
  }
  def map( f:A => A) = this
}
case class Defined[A]( dt:A ) extends InfiniteInstant[A] {
  val isPositiveInfinity = false
  val isNegativeInfinity = false
  def isBefore( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case PosInfinity()  => true
    case NegInfinity()  => false
    case Defined(dt2) => ci.isBefore(dt,dt2)
  }
  def isAfter( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case PosInfinity()  => false
    case NegInfinity()  => true
    case Defined(dt2) => ci.isAfter(dt,dt2)
  }
  def isEqual( otherPg: InfiniteInstant[A] )( implicit ci:Instant[A] ) = otherPg match {
    case Defined(dt2) => ci.isEqual(dt,dt2)
    case _ => false
  }
  def map( f:A => A ) = Defined( f(dt) )
}

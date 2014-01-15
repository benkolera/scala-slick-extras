package com.benkolera.slick.joda

trait Instant[A] {
  def isBefore( x: A , y: A ):Boolean
  def isAfter( x: A , y:A ):Boolean
  def isEqual( x: A , y:A ):Boolean
  def getMillis( x: A ):Long
  def isBeforeOrEqual( x: A , y:A ):Boolean = {
    isEqual(x,y) || isBefore(x,y)
  }
  def isAfterOrEqual( x:A , y:A ):Boolean = {
    isEqual(x,y) || isAfter(x,y)
  }
}

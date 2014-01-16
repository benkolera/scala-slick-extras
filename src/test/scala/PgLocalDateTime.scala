package com.benkolera.slick

import org.specs2.mutable._
import org.joda.time.LocalDateTime
import com.benkolera.slick.joda.{PosInfinity,NegInfinity,Defined}
import com.benkolera.slick.joda.pg.{pgLocalDateTimeInstant}

class PgLocalDateTimeSpec extends Specification {

  "PgLocalDateTime.isAfter" should {
    "Return false for NegInfinity and Anything" in {
      NegInfinity().isAfter( PosInfinity() ) must_==( false )
      NegInfinity().isAfter( NegInfinity() ) must_==( false )
      NegInfinity().isAfter( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return true for PosInfinity and Anything but PosInfinity" in {
      PosInfinity().isAfter( NegInfinity() ) must_==( true )
      PosInfinity().isAfter( PosInfinity() ) must_==( false )
      PosInfinity().isAfter( Defined(new LocalDateTime) ) must_==( true )
    }
    "Return true for Neg or a defined that it is after" in {
      val defined = Defined(new LocalDateTime)
      defined.isAfter( NegInfinity() ) must_==( true )
      defined.isAfter( PosInfinity() ) must_==( false )
      defined.isAfter( defined ) must_==( false )
      defined.isAfter( defined.map( _.plusDays(1) ) ) must_==( false )
      defined.isAfter( defined.map( _.plusDays(-1) ) ) must_==( true )
    }
  }

  "PgLocalDateTime.isBefore" should {
    "Return true for NegInfinity and Anything except NegInfinity" in {
      NegInfinity().isBefore( PosInfinity() ) must_==( true )
      NegInfinity().isBefore( NegInfinity() ) must_==( false )
      NegInfinity().isBefore( Defined(new LocalDateTime) ) must_==( true )
    }
    "Return false for everything" in {
      PosInfinity().isBefore( NegInfinity() ) must_==( false )
      PosInfinity().isBefore( PosInfinity() ) must_==( false )
      PosInfinity().isBefore( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return true for Pos or a defined that it is before" in {
      val defined = Defined(new LocalDateTime)
      defined.isBefore( NegInfinity() ) must_==( false )
      defined.isBefore( PosInfinity() ) must_==( true )
      defined.isBefore( defined ) must_==( false )
      defined.isBefore( defined.map( _.plusDays(1) ) ) must_==( true )
      defined.isBefore( defined.map( _.plusDays(-1) ) ) must_==( false )
    }
  }

  "PgLocalDateTime.isEqual" should {
    "Return true only for NegInfinity" in {
      NegInfinity().isEqual( PosInfinity() ) must_==( false )
      NegInfinity().isEqual( NegInfinity() ) must_==( true )
      NegInfinity().isEqual( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return false only for posinfinity" in {
      PosInfinity().isEqual( NegInfinity() ) must_==( false )
      PosInfinity().isEqual( PosInfinity() ) must_==( true )
      PosInfinity().isEqual( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return true for defined with the same date" in {
      val defined = Defined(new LocalDateTime)
      defined.isEqual( NegInfinity() ) must_==( false )
      defined.isEqual( PosInfinity() ) must_==( false )
      defined.isEqual( defined ) must_==( true )
      defined.isEqual( defined.map( _.plusDays(1) ) ) must_==( false )
      defined.isEqual( defined.map( _.plusDays(-1) ) ) must_==( false )
    }
  }

  "PgLocalDateTime.isBeforeOrEqual" should {
    "Return true always" in {
      NegInfinity().isBeforeOrEqual( PosInfinity() ) must_==( true )
      NegInfinity().isBeforeOrEqual( NegInfinity() ) must_==( true )
      NegInfinity().isBeforeOrEqual( Defined(new LocalDateTime) ) must_==( true )
    }
    "Return false for all but PosInfinity()" in {
      PosInfinity().isBeforeOrEqual( NegInfinity() ) must_==( false )
      PosInfinity().isBeforeOrEqual( PosInfinity() ) must_==( true )
      PosInfinity().isBeforeOrEqual( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return true for defined with the same date, defined with greater dt or Pos" in {
      val defined = Defined(new LocalDateTime)
      defined.isBeforeOrEqual( NegInfinity() ) must_==( false )
      defined.isBeforeOrEqual( PosInfinity() ) must_==( true )
      defined.isBeforeOrEqual( defined ) must_==( true )
      defined.isBeforeOrEqual( defined.map( _.plusDays(1) ) ) must_==( true )
      defined.isBeforeOrEqual( defined.map( _.plusDays(-1) ) ) must_==( false )
    }
  }

  "PgLocalDateTime.isAfterOrEqual" should {
    "Return false except for NegInfinity()" in {
      NegInfinity().isAfterOrEqual( PosInfinity() ) must_==( false )
      NegInfinity().isAfterOrEqual( NegInfinity() ) must_==( true )
      NegInfinity().isAfterOrEqual( Defined(new LocalDateTime) ) must_==( false )
    }
    "Return true always" in {
      PosInfinity().isAfterOrEqual( NegInfinity() ) must_==( true )
      PosInfinity().isAfterOrEqual( PosInfinity() ) must_==( true )
      PosInfinity().isAfterOrEqual( Defined(new LocalDateTime) ) must_==( true )
    }
    "Return true for defined with the same date, defined with lesser dt or Neg" in {
      val defined = Defined(new LocalDateTime)
      defined.isAfterOrEqual( NegInfinity() ) must_==( true )
      defined.isAfterOrEqual( PosInfinity() ) must_==( false )
      defined.isAfterOrEqual( defined ) must_==( true )
      defined.isAfterOrEqual( defined.map( _.plusDays(1) ) ) must_==( false )
      defined.isAfterOrEqual( defined.map( _.plusDays(-1) ) ) must_==( true )
    }
  }

}

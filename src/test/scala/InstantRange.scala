package com.benkolera.slick.joda

import pg.pgLocalDateTimeInstant
import org.specs2.mutable._
import org.joda.time.LocalDateTime
import scala.util.Try

class InstantRangeSpec extends Specification {
  val now = LocalDateTime.now()
  val tomorrow = now.plusDays(1)
  val yesterday = now.plusDays(-1)

  val yes2tomInc = InstantRange(
    InclusiveStart( Defined(yesterday) ),
    InclusiveEnd( Defined(tomorrow) )
  )

  val yes2tomExc = InstantRange(
    ExclusiveStart( Defined(yesterday) ),
    ExclusiveEnd( Defined(tomorrow) )
  )

  val now2tomInc = InstantRange(
    InclusiveStart( Defined(now) ),
    InclusiveEnd( Defined(tomorrow) )
  )

  "InstantRange" should {
    "error on start > end" in {
      Try(InstantRange(
        InclusiveStart( PosInfinity() ),
        InclusiveEnd( NegInfinity() )
      )) must beFailedTry
    }

    "create empty" in {
      InstantRange(
        InclusiveStart( Defined(now) ),
        ExclusiveEnd( Defined(now) )
      ) must_==( EmptyRange() )
    }
  }

  "InstantRange.contains" should {
    "contain timestamp" in {
      yes2tomInc.contains( Defined(now) ) must_==( true )
    }

    "not contain infinity" in {
      yes2tomInc.contains( PosInfinity[LocalDateTime]() ) must_==( false )
      yes2tomInc.contains( NegInfinity[LocalDateTime]() ) must_==( false )
    }

    "contain range" in {
      yes2tomInc.contains( now2tomInc ) must_==( true )
    }

    "not contain range if exclusive" in {
      yes2tomExc.contains( now2tomInc ) must_==( false )
    }

    "not contain empty" in {
      yes2tomInc.contains( EmptyRange[LocalDateTime]() ) must_==( false )
    }
  }

  "InstantRange.overlaps" should {
    "overlap range" in {
      yes2tomInc.overlaps( now2tomInc ) must_==( true )
      yes2tomExc.overlaps( now2tomInc ) must_==( true )
    }

    "not overlap empty" in {
      yes2tomInc.overlaps( EmptyRange[LocalDateTime]() ) must_==( false )
    }
  }
}

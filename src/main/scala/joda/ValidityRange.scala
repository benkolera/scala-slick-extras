package com.benkolera.slick.joda

object ValidityRange {
  def apply[A:Instant](start:InfiniteInstant[A],end:InfiniteInstant[A]) = {
    InstantRange( InclusiveStart(start), ExclusiveEnd(end) )
  }
}

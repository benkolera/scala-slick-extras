package com.benkolera.slick.joda

import org.postgresql.PGStatement

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
}

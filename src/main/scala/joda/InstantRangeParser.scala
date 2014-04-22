package com.benkolera.slick.joda

import java.sql.Timestamp
import scala.util.parsing.combinator.RegexParsers

case class InstantRangeParser[A:Instant](
  f:Timestamp => InfiniteInstant[A]
) extends RegexParsers {
  def pinf = "infinity".r ^^ { _ => PosInfinity[A]() }
  def ninf = "-infinity".r ^^ { _ => NegInfinity[A]() }
  def tz   = """[+-]?\d{0,2}""".r
  def date = "\"" ~> """\d{4}-\d{1,2}-\d{1,2} \d\d:\d\d:\d\d\.?\d*""".r <~ (tz ~ "\"") ^^ {
    d => f( Timestamp.valueOf(d) )
  }
  def pgdt = pinf | ninf | date

  def open = """[\(\[]""".r
  def close = """[\)\]]""".r

  def incOpen = open ^^ { _ == "[" }
  def emptyOpen = open ^^ { _ => EmptyStart[A]() }
  def incClose = close ^^ { _ == "]" }
  def emptyClose = close ^^ { _ => EmptyEnd[A]() }

  def fst =
    ( incOpen ~ pgdt ) ^^ { case inc ~ date =>
      if (inc) InclusiveStart[A](date) else ExclusiveStart[A](date)
    }

  def validityFst =
    ( incOpen ~ pgdt ) ^^ { case inc ~ date =>
      if (inc) date else throw new Exception("Exclusive start not possible in validity")
    }

  def scd =
    ( pgdt ~ incClose ) ^^ { case date ~ inc =>
      if (inc) InclusiveEnd[A](date) else ExclusiveEnd[A](date)
    }

  def validityScd =
    ( pgdt ~ incClose ) ^^ { case date ~ inc =>
      if (!inc) date else throw new Exception("Inclusive end not possible in validity")
    }

  def defined =
    ( (fst | emptyOpen) ~ "," ~ (scd | emptyClose) ) ^^ {
      case fst ~ _ ~ scd => InstantRange[A]( fst, scd )
    }

  def validityDefined =
    ( validityFst ~ "," ~ validityScd ) ^^ {
      case fst ~ _ ~ scd => ValidityRange[A]( fst, scd )
    }

  def empty = "empty".r ^^ { _ => EmptyRange[A]() }
  def range = defined | empty
  def validityRange = validityDefined

  def fromSql( sql:String ): InstantRange[A] = {
    parseAll(range, sql) match {
      case Success(result, _) => result
      case n: NoSuccess => throw new Exception( s"Cannot parse $sql as InstantRange: ${n.msg}" )
    }
  }

  def validityFromSql( sql:String ): ValidityRange[A] = {
    parseAll(validityRange, sql) match {
      case Success(result, _) => result
      case n: NoSuccess => throw new Exception( s"Cannot parse $sql as ValidityRange: ${n.msg}" )
    }

  }
}

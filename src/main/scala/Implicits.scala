package com.benkolera.slick

import java.nio.file.{Path,Paths}
import org.joda.time.{LocalDateTime,LocalTime,LocalDate,DateTime}
import java.sql.{Date,Time,Timestamp}
import scala.slick.jdbc.{GetResult,SetParameter}
import GetResult._
import SetParameter._
import joda.pg._
import PgLocalDateTime._
import PgDateTime._

object Implicits {
  //============================================================================
  //  java.nio.file.Path
  //============================================================================
  implicit val pathSetParameters = SetParameter.apply[Path]{ (p,params) =>
    params.setString(p.toString)
  }
  implicit val pathGetResult = GetResult[Path]( r =>
    Paths.get( r.nextString )
  )
  implicit val pathOptSetParameters = SetParameter.apply[Option[Path]]{
    (p,params) => params.setStringOption(p.map(_.toString))
  }
  implicit val pathOptGetResult = GetResult[Option[Path]]( r =>
    r.nextStringOption.map( Paths.get( _ ) )
  )

  //============================================================================
  //  PgLocalDateTime
  //============================================================================

  implicit val pgLocalDateTimeSetParameters = SetParameter.apply[PgLocalDateTime]{
    (ldt,params) => params.setTimestamp(PgLocalDateTime.toSql(ldt))
  }
  implicit val pgLocalDateTimeGetResult = GetResult[PgLocalDateTime]( r =>
    PgLocalDateTime.fromSql( r.nextTimestamp )
  )

  // I'd argue that it doesn't make sense to have a timestamp that is both
  // nullable and possible to be infinite, but it doesn't hurt to have this in
  // here.
  implicit val pgLocalDateTimeOptSetParameters =
    SetParameter.apply[Option[PgLocalDateTime]]{ (ldt,params) =>
      params.setTimestampOption( ldt.map( PgLocalDateTime.toSql _ ) )
    }
  implicit val pgLocalDateTimeOptGetResult = GetResult[Option[PgLocalDateTime]](
    r => r.nextTimestampOption.map( PgLocalDateTime.fromSql _ )
  )


  //============================================================================
  //  org.joda.time.LocalDateTime
  //============================================================================

  private def ldtToSql( ldt:LocalDateTime ):Timestamp = {
    new java.sql.Timestamp(ldt.toDateTime.getMillis)
  }

  private def sqlToLdt( sql:Timestamp ):LocalDateTime = {
    joda.pg.PgLocalDateTime.fromSql( sql ) match {
      case joda.Defined(dt) => dt
      case _ => throw new RuntimeException(
        "PgLocalDateTime must be used for Infinite timestamp."
      )
    }
  }

  implicit val localDateTimeSetParameters = SetParameter.apply[LocalDateTime]{
    (ldt,params) => params.setTimestamp( ldtToSql(ldt) )
  }
  implicit val localDateTimeGetResult = GetResult[LocalDateTime]( r =>
    sqlToLdt( r.nextTimestamp )
  )
  implicit val localDateTimeOptSetParameters =
    SetParameter.apply[Option[LocalDateTime]]{ (ldt,params) =>
      params.setTimestampOption( ldt.map( ldtToSql _ ) )
    }
  implicit val localDateTimeOptGetResult = GetResult[Option[LocalDateTime]](
    r => r.nextTimestampOption.map( sqlToLdt _ )
  )

  //============================================================================
  //  PgDateTime
  //============================================================================

  implicit val pgDateTimeSetParameters = SetParameter.apply[PgDateTime]{
    (dt,params) => params.setTimestamp(PgDateTime.toSql(dt))
  }
  implicit val pgDateTimeGetResult = GetResult[PgDateTime]( r =>
    PgDateTime.fromSql( r.nextTimestamp )
  )

  // I'd argue that it doesn't make sense to have a timestamp that is both
  // nullable and possible to be infinite, but it doesn't hurt to have this in
  // here.
  implicit val pgDateTimeOptSetParameters =
    SetParameter.apply[Option[PgDateTime]]{ (dt,params) =>
      params.setTimestampOption( dt.map( PgDateTime.toSql _ ) )
    }
  implicit val pgDateTimeOptGetResult = GetResult[Option[PgDateTime]](
    r => r.nextTimestampOption.map( PgDateTime.fromSql _ )
  )

  //============================================================================
  //  org.joda.time.DateTime
  //============================================================================

  private def dtToSql( dt:DateTime ):Timestamp = {
    joda.pg.PgDateTime.toSql( joda.Defined(dt) )
  }

  private def sqlToDt( sql:Timestamp ):DateTime = {
    joda.pg.PgDateTime.fromSql( sql ) match {
      case joda.Defined(dt) => dt
      case _ => throw new RuntimeException(
        "PgLocalDateTime must be used for Infinite timestamp."
      )
    }
  }

  implicit val dateTimeSetParameters = SetParameter.apply[DateTime]{
    (dt,params) => params.setTimestamp( dtToSql(dt) )
  }
  implicit val dateTimeGetResult = GetResult[DateTime]( r =>
    sqlToDt( r.nextTimestamp )
  )
  implicit val dateTimeOptSetParameters =
    SetParameter.apply[Option[DateTime]]{ (dt,params) =>
      params.setTimestampOption( dt.map( dtToSql _ ) )
    }
  implicit val dateTimeOptGetResult = GetResult[Option[DateTime]](
    r => r.nextTimestampOption.map( sqlToDt _ )
  )

  //============================================================================
  //  org.joda.time.LocalDate
  //============================================================================
  private def ldToSql( ld:LocalDate ):Date = {
    new java.sql.Date(ld.toDateTimeAtStartOfDay.getMillis)
  }

  private def sqlToLd( sql:Date ):LocalDate = {
    new LocalDate( sql.getTime )
  }

  implicit val localDateSetParameters = SetParameter.apply[LocalDate]{
    (ld,params) => params.setDate( ldToSql(ld) )
  }
  implicit val localDateGetResult = GetResult[LocalDate]( r =>
    sqlToLd( r.nextDate )
  )
  implicit val localDateOptSetParameters =
    SetParameter.apply[Option[LocalDate]]{ (ld,params) =>
      params.setDateOption( ld.map( ldToSql _ ) )
    }
  implicit val localDateOptGetResult = GetResult[Option[LocalDate]](
    r => r.nextDateOption.map( sqlToLd _ )
  )

  //============================================================================
  //  org.joda.time.LocalTime
  //============================================================================

  private def ltToSql( lt:LocalTime ):Time = {
    new java.sql.Time(lt.toDateTimeToday().getMillis())
  }
  private def sqlToLt( sql:Time ):LocalTime = {
    new LocalTime( sql.getTime )
  }

  implicit val localTimeSetParameters = SetParameter.apply[LocalTime]{
    (lt,params) => params.setTime( ltToSql(lt) )
  }
  implicit val localTimeGetResult = GetResult[LocalTime]( r =>
    sqlToLt( r.nextTime )
  )
  implicit val localTimeOptSetParameters =
    SetParameter.apply[Option[LocalTime]]{ (lt,params) =>
      params.setTimeOption( lt.map( ltToSql _ ) )
    }
  implicit val localTimeOptGetResult = GetResult[Option[LocalTime]](
    r => r.nextTimeOption.map( sqlToLt _ )
  )

  //============================================================================
  //  PgLocalDateTimeRange
  //============================================================================

  object PgRange extends pg.PgOptConv[PgLocalDateTimeRange]{
    val pgType = "tstzrange"
    def fromSql( s:String ) = PgLocalDateTime.validityRangeFromSql(s)
    def toSql( a:PgLocalDateTimeRange ) = PgLocalDateTime.rangeToSql(a)
  }
  implicit val pgLocalDateTimeRangeSetParameters = PgRange.setPgParameter
  implicit val pgLocalDateTimeRangeGetResult = PgRange.getPgResult
  implicit val pgLocalDateTimeRangeOptSetParameters = PgRange.setPgOptionParameter
  implicit val pgLocalDateTimeRangeOptGetResult = PgRange.getPgOptionResult

  //============================================================================
  //  Case class helpers getResultCaseClass{2..22}
  //============================================================================

  def getResultCaseClass2[T1:GetResult,T2:GetResult,A](cons:(T1,T2) => A ) = {
    GetResult.createGetTuple2[T1,T2] andThen cons.tupled
  }

  def getResultCaseClass3[T1:GetResult,T2:GetResult,T3:GetResult,A](cons:(T1,T2,T3) => A ) = {
    GetResult.createGetTuple3[T1,T2,T3] andThen cons.tupled
  }

  def getResultCaseClass4[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,A](cons:(T1,T2,T3,T4) => A) = {
    GetResult.createGetTuple4[T1,T2,T3,T4] andThen cons.tupled
  }

  def getResultCaseClass5[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,A](cons:(T1,T2,T3,T4,T5) => A) = {
    GetResult.createGetTuple5[T1,T2,T3,T4,T5] andThen cons.tupled
  }

  def getResultCaseClass6[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,A](cons:(T1,T2,T3,T4,T5,T6) => A) = {
    GetResult.createGetTuple6[T1,T2,T3,T4,T5,T6] andThen cons.tupled
  }

  def getResultCaseClass7[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7) => A) = {
    GetResult.createGetTuple7[T1,T2,T3,T4,T5,T6,T7] andThen cons.tupled
  }

  def getResultCaseClass8[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8) => A) = {
    GetResult.createGetTuple8[T1,T2,T3,T4,T5,T6,T7,T8] andThen cons.tupled
  }

  def getResultCaseClass9[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9) => A) = {
    GetResult.createGetTuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9] andThen cons.tupled
  }

  def getResultCaseClass10[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => A) = {
    GetResult.createGetTuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] andThen cons.tupled
  }

  def getResultCaseClass11[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) => A) = {
    GetResult.createGetTuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] andThen cons.tupled
  }

  def getResultCaseClass12[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) => A) = {
    GetResult.createGetTuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] andThen cons.tupled
  }

  def getResultCaseClass13[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) => A) = {
    GetResult.createGetTuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] andThen cons.tupled
  }

  def getResultCaseClass14[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) => A) = {
    GetResult.createGetTuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] andThen cons.tupled
  }

  def getResultCaseClass15[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) => A) = {
    GetResult.createGetTuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] andThen cons.tupled
  }

  def getResultCaseClass16[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) => A) = {
    GetResult.createGetTuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] andThen cons.tupled
  }

  def getResultCaseClass17[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) => A) = {
    GetResult.createGetTuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] andThen cons.tupled
  }

  def getResultCaseClass18[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,T18:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) => A) = {
    GetResult.createGetTuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] andThen cons.tupled
  }

  def getResultCaseClass19[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,T18:GetResult,T19:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) => A) = {
    GetResult.createGetTuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] andThen cons.tupled
  }

  def getResultCaseClass20[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,T18:GetResult,T19:GetResult,T20:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) => A) = {
    GetResult.createGetTuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] andThen cons.tupled
  }

  def getResultCaseClass21[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,T18:GetResult,T19:GetResult,T20:GetResult,T21:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) => A) = {
    GetResult.createGetTuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] andThen cons.tupled
  }

  def getResultCaseClass22[T1:GetResult,T2:GetResult,T3:GetResult,T4:GetResult,T5:GetResult,T6:GetResult,T7:GetResult,T8:GetResult,T9:GetResult,T10:GetResult,T11:GetResult,T12:GetResult,T13:GetResult,T14:GetResult,T15:GetResult,T16:GetResult,T17:GetResult,T18:GetResult,T19:GetResult,T20:GetResult,T21:GetResult,T22:GetResult,A](cons:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) => A) = {
    GetResult.createGetTuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22] andThen cons.tupled
  }

}

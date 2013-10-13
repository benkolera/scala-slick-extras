package com.benkolera.slick

import java.nio.file.{Path,Paths}
import org.joda.time.{LocalDateTime,LocalTime,LocalDate}

import scala.slick.jdbc.{GetResult,SetParameter}
import GetResult._
import SetParameter._

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
  //  org.joda.time.LocalDateTime
  //============================================================================
  implicit val localDateTimeSetParameters = SetParameter.apply[LocalDateTime]{
    (ldt,params) => params.setTimestamp(
      new java.sql.Timestamp(ldt.toDateTime.getMillis)
    )
  }
  implicit val localDateTimeGetResult = GetResult[LocalDateTime]( r =>
    new LocalDateTime( r.nextTimestamp.getTime )
  )
  implicit val localDateTimeOptSetParameters =
    SetParameter.apply[Option[LocalDateTime]]{ (ldt,params) =>
      params.setTimestampOption(
        ldt.map( x => new java.sql.Timestamp(x.toDateTime.getMillis))
      )
    }
  implicit val localDateTimeOptGetResult = GetResult[Option[LocalDateTime]](
    r => r.nextTimestampOption.map( ts => new LocalDateTime( ts.getTime ) )
  )

  //============================================================================
  //  org.joda.time.LocalDate
  //============================================================================
  implicit val localDateSetParameters = SetParameter.apply[LocalDate]{
    (ld,params) => params.setDate(
      new java.sql.Date(ld.toDateTimeAtStartOfDay.getMillis)
    )
  }
  implicit val localDateGetResult = GetResult[LocalDate]( r =>
    new LocalDate( r.nextDate.getTime )
  )
  implicit val localDateOptSetParameters =
    SetParameter.apply[Option[LocalDate]]{ (ld,params) =>
      params.setDateOption(
        ld.map( x => new java.sql.Date(x.toDateTimeAtStartOfDay.getMillis))
      )
    }
  implicit val localDateOptGetResult = GetResult[Option[LocalDate]](
    r => r.nextDateOption.map( d => new LocalDate( d.getTime ) )
  )

  //============================================================================
  //  org.joda.time.LocalTime
  //============================================================================
  implicit val localTimeSetParameters = SetParameter.apply[LocalTime]{
    (lt,params) => params.setTime(
      new java.sql.Time(lt.toDateTimeToday().getMillis())
    )
  }
  implicit val localTimeGetResult = GetResult[LocalTime]( r =>
    new LocalTime( r.nextTime.getTime )
  )
  implicit val localTimeOptSetParameters =
    SetParameter.apply[Option[LocalTime]]{ (lt,params) =>
      params.setTimeOption(
        lt.map( x => new java.sql.Time(x.toDateTimeToday.getMillis))
      )
    }
  implicit val localTimeOptGetResult = GetResult[Option[LocalTime]](
    r => r.nextTimeOption.map( t => new LocalTime( t.getTime ) )
  )

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

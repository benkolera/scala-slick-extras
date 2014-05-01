package com.benkolera.slick

import scala.slick.jdbc.{
  PositionedParameters,
  PositionedResult,
  SetParameter,
  GetResult
}
import argonaut.{EncodeJson,DecodeJson,Parse}

// TODO: Calling this Argonaut could turn out to be a stupid idea given that
// the convention of argonaut is to import Argonaut._ to get argonaut.Argonaut._
object Argonaut {

  def jsonGetterSetter[A](implicit e:EncodeJson[A],d:DecodeJson[A]) =
    DbGetterSetter[A](
      jsonGetResult[A].apply _ ,
      jsonSetParameter[A].apply _
    )

  def jsonSetParameter[A](implicit e:EncodeJson[A]) =
    SetParameter.apply[A]{
      (a,params) => params.setString( e.encode(a).spaces2 )
    }

  def jsonGetResult[A:DecodeJson] = GetResult[A]( r =>
    Parse.decodeValidation[A]( r.nextString ).fold(
      errs => throw new RuntimeException(s"Failed parsing JSON: $errs"),
      identity
    )
  )

  def optionalJsonGetterSetter[A](implicit e:EncodeJson[A],d:DecodeJson[A]) =
    DbGetterSetter[Option[A]](
      optionalJsonGetResult[A].apply _ ,
      optionalJsonSetParameter[A].apply _
    )

  def optionalJsonSetParameter[A](implicit e:EncodeJson[A]) =
    SetParameter.apply[Option[A]]{
      (a,params) => a.foreach(x => params.setString( e.encode(x).spaces2 ))
    }

  def optionalJsonGetResult[A:DecodeJson] = GetResult[Option[A]]( r =>
    r.nextStringOption.fold(Option.empty[A])(x =>
      Parse.decodeValidation[A]( x ).fold(
        errs => throw new RuntimeException(s"Failed parsing JSON: $errs"),
        Some(_)
      )
    )
  )

}

package com.benkolera.slick

import scala.slick.jdbc.{
  PositionedParameters,
  PositionedResult,
  SetParameter,
  GetResult
}
import argonaut.{EncodeJson,DecodeJson,Parse}

trait DbGetterSetter[A] extends SetParameter[A] with GetResult[A] {
  def getResult( res:PositionedResult ): A
  def setParameter( a:A , params:PositionedParameters ): Unit

  def apply( res:PositionedResult ):A = getResult( res )
  def apply( a:A , ps:PositionedParameters ):Unit = setParameter( a, ps )
}

object DbGetterSetter {
  def apply[A](
    getter: PositionedResult => A,
    setter: (A,PositionedParameters) => Unit
  ) = new DbGetterSetter[A]{
    def getResult( res:PositionedResult ) = getter( res )
    def setParameter( a:A , ps:PositionedParameters ) = setter( a , ps )
  }
}

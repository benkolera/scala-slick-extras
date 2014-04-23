package com.benkolera.slick.pg

import scala.slick.jdbc.{PositionedResult,GetResult}

trait DecodePgObject[A] extends GetResult[A] {
  def apply(pr:PositionedResult) =
    stringDecode( pr.nextString )

  def stringDecode(s:String): A
}

object DecodePgObject extends DecodePgObjects {
  def apply[A](f:String => A): DecodePgObject[A] =
    new DecodePgObject[A]{
      def stringDecode(s:String) = f(s)
    }
}

trait DecodePgObjects {

  implicit def decodeSqlOption[A:DecodePgObject]: GetResult[Option[A]] =
    GetResult{ (pr:PositionedResult) =>
      pr.nextStringOption.map(
        implicitly[DecodePgObject[A]].stringDecode(_)
      )
    }

  implicit def decodeSqlList[A:DecodePgObject]: GetResult[List[A]] =
    DecodePgObject{ (s:String) =>
      s.tail.init.split(",").toList match {
        case ""::Nil => Nil
        case x       => x.map( implicitly[DecodePgObject[A]].stringDecode(_) )
      }
    }

}

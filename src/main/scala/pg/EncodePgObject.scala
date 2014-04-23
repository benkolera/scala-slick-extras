package com.benkolera.slick.pg

import scala.slick.jdbc.{PositionedParameters,SetParameter}
import java.sql.Types

trait EncodePgObject[A] extends SetParameter[A] {
  def apply(a:A, pp:PositionedParameters) =
    pp.setObject(
      PgObject( typeName, stringEncode(a) )
    , Types.OTHER
    )

  def stringEncode(a:A): String
  val typeName: String
}

object EncodePgObject extends EncodePgObjects {
  def apply[A](n:String)(f:A => String): EncodePgObject[A] =
    new EncodePgObject[A]{
      def stringEncode(a:A) = f(a)
      val typeName = n
    }
}

trait EncodePgObjects {

  implicit def encodeSqlOption[A:EncodePgObject]: SetParameter[Option[A]] =
    SetParameter{ case (a:Option[A],pp:PositionedParameters) =>
      a match {
        case Some(x) => implicitly[EncodePgObject[A]].apply( x, pp )
        case None    => pp.setNull( Types.OTHER )
      }
    }

  implicit def encodeSqlList[A:EncodePgObject]: SetParameter[List[A]] = {
    val codec = implicitly[EncodePgObject[A]]
    EncodePgObject( "_" + codec.typeName ){ (a:List[A]) =>
      "{" + a.map( codec.stringEncode(_) ).mkString(",") + "}"
    }
  }

}

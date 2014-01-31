package com.benkolera.slick.pg

import org.postgresql.util.PGobject

trait PgTrait extends PGobject {
  val pgType: String
  val pgValue: String

  setType(pgType)
  override def getValue = pgValue
}

case class PgObject ( pgType:String, pgValue:String ) extends PgTrait

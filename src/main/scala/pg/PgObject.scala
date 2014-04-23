package com.benkolera.slick.pg

import org.postgresql.util.PGobject

case class PgObject(pgType:String, pgValue:String) extends PGobject {
  setType(pgType)
  override def getValue = pgValue
}

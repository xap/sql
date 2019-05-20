package com.gigaspaces.parser

trait SQL

object SQL {

  case class Select(field: Field, table: Table) extends SQL


  trait Table
  case class SimpleTable(name: String) extends Table

  trait Field
  case object AllFields extends Field

}


package com.gigaspaces.parser

sealed trait SQL

object SQL {

  case class TableName(name: String, alias : Option[String])

  case class Select(field: Fields, table: Tables) extends SQL


  sealed trait Tables
  case class TableNames(names : List[TableName]) extends Tables

  case class Field(table : Option[String], name: String)

  sealed trait Fields
  case class AllFields() extends Fields
  case class SomeFields(fields : List[Field]) extends Fields

}


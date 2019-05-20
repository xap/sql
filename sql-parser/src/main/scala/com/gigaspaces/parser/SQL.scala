package com.gigaspaces.parser

sealed trait SQL

object SQL {

  case class TableName(name: String, alias : Option[String])

  case class Select(field: Fields, table: Tables) extends SQL


  sealed trait Tables
  case class TableNames(names : List[TableName]) extends Tables

  sealed trait Fields
  case object AllFields extends Fields

}


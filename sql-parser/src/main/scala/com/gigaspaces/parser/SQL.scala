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

  sealed trait Exp
  case class Number(value : Double) extends Exp
  case class StringLiteral(value: String) extends Exp
  case class Variable(namespace: List[String], value: String) extends Exp
  case class Function(name: String, args: List[Exp]) extends Exp
  case class BinaryOp(left: Exp, op : String, right: Exp) extends Exp
  case class UnaryOp(name: String, a1 : Exp) extends Exp

}

sealed trait Arith

object Arith{
  case class Number(value : Double) extends Arith
  case class Grouped(a: Arith) extends Arith
  case class BinaryOp(a1 : Arith, name: String, a2: Arith) extends Arith
  case class UnaryOp(name: String, a1 : Arith) extends Arith
}
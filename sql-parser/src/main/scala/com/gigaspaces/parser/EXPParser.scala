package com.gigaspaces.parser

import com.gigaspaces.parser.SQL.Exp

import scala.language.{higherKinds, implicitConversions}


object EXPParser {

  def expParser[Parser[+ _]](P: Parsers[Parser]): Parser[Exp] = {
    import P.{string => _, _}
    import SQL._
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def number: Parser[Number] = P.double map Number scope "number"
    def stringLiteral : Parser[StringLiteral] = P.escapedQuoted map StringLiteral scope "string literal"

    def variable : Parser[Variable] = P.sep1(P.name, P.string(".")) map createVariable scope "variable"
    def createVariable(lst: List[String]) : Variable = lst match {
      case name::Nil => Variable(List(), name)
      case l@_::_ => Variable(l.init, l.last)
      case _ => throw new IllegalStateException("can't possible be")
    }

    def function : Parser[Function] = map2(token(P.name),  argList)(Function) scope "function"

    def argList : Parser[List[Exp]] = surround("(", ")"){
      tok("*").as(List(Variable(List(), "*"))) | ((exp <* whitespace) sep ",")
    } scope "arg list"

    def exp: Parser[Exp] =  number | stringLiteral | attempt(function) | variable | surround("(", ")")(exp)

    def binaryOpL(op: Parser[String]): Parser[Exp] = opL(exp)(op.map(combine))
    def combine(op: String)(left: Exp, right: Exp): Exp = Op(left, op, right)
    def binaryOps: Parser[String] = "*" | "+" | "/" | "<" | "<=" | ">" | "=>"
    def compositExp: Parser[Exp] = binaryOpL(binaryOps)

    root(compositExp)
  }
}

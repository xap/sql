package com.gigaspaces.parser



import scala.language.{higherKinds, implicitConversions}


object EXPParser {

  import com.gigaspaces.parser.SQL.Exp

  def expParser[Parser[+ _]](P: Parsers[Parser]): Parser[Exp] = {
    import P.{string => _, _}
    import SQL._
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def number: Parser[Number] = P.double map Number scope "number"

    def stringLiteral: Parser[StringLiteral] = P.escapedQuoted map StringLiteral scope "string literal"

    def variable: Parser[Variable] = P.sep1(P.name, P.string(".")) map createVariable scope "variable"

    def createVariable(lst: List[String]): Variable = lst match {
      case name :: Nil => Variable(List(), name)
      case l@_ :: _ => Variable(l.init, l.last)
      case _ => throw new IllegalStateException("can't possible be")
    }

    def function: Parser[Function] = map2(token(P.name), argList)(Function) scope "function"

    def argList: Parser[List[Exp]] = surround("(", ")") {
      tok("*").as(List(Variable(List(), "*"))) | ((precedence(0) <* whitespace) sep ",")
    } scope "arg list"

    def exp: Parser[Exp] = number | stringLiteral | attempt(function) | variable | surround("(", ")")(precedence(0)) | map2("-", precedence(2))(UnaryOp)

    def combine(op: String)(left: Exp, right: Exp): Exp = BinaryOp(left, op, right)

    def precedence0: Parser[String] = "="
    def precedence1: Parser[String] = "+" | "-"
    def precedence2: Parser[String] = "*" | "/"
    def precedence3: Parser[String] = "^"

    def precedence(n: Int): Parser[Exp] = n match {
      case 0 => opL(precedence(1))(precedence0.map(combine)) scope "E (precedence0)"
      case 1 => opL(precedence(2))(precedence1.map(combine)) scope "T (precedence1)"
      case 2 => opL(precedence(3))(precedence2.map(combine)) scope "F (precedence2)"
      case 3 => opL(exp)(precedence3.map(combine)) scope "G (precedence3)"
      case p => fail("precedence " + p + " not exists")
    }
    root(precedence(0))
  }


  def arith[Parser[+ _]](P: Parsers[Parser]): Parser[Arith] = {

    // this is cool, parsing arithmetic exp according to math precedence (left associative)
    // see ArithTest
    // https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

    import P.{string => _, _}
    import Arith._
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    /*
    E --> T {( "+" | "-" ) T}
    T --> F {( "*" | "/" ) F}
    F --> P ["^" F]
    P --> v | "(" E ")" | "-" T
     */

    def precedence0: Parser[String] = "+" | "-"
//
    def precedence1: Parser[String] = "*" | "/"
//
    def precedence2: Parser[String] = "^"

    def combine(op: String)(left: Arith, right: Arith): Arith = Arith.BinaryOp(left, op, right)

    def number: Parser[Number] = double map Number scope "number"

//    root(e)

    def precedence(n: Int): Parser[Arith] = n match {
      case 0 => opL(precedence(1))(precedence0.map(combine)) scope "E (precedence0)"
      case 1 => opL(precedence(2))(precedence1.map(combine)) scope "T (precedence1)"
      case 2 => opL(p)(precedence2.map(combine)) scope "F (precedence2)"
      case p => fail("precedence " + p + " not exists")
    }
    def p: Parser[Arith] = number | surround("(", ")")(precedence(0)) | map2("-", precedence(2))(UnaryOp)

    root(precedence(0))

  }
}

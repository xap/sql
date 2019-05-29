package com.gigasapces.parser

import com.gigaspaces.parser.SQL._
import com.gigaspaces.parser.instances.Reference
import com.gigaspaces.parser.{EXPParser, SQL}
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.scalatest.Matchers._

// https://alvinalexander.com/scala/scalatest-tutorials-from-scala-cookbook
// http://www.scalatest.org/at_a_glance/FlatSpec

class ExpTest extends FunSpec with BeforeAndAfter {

  import com.gigaspaces.parser.instances.ReferenceTypes.Parser

  val P: Reference.type = com.gigaspaces.parser.instances.Reference
  val sql: Parser[Exp] = EXPParser.expParser(P)

  describe("testing numeric expression") {
    it("double is an Number expression") {
      val exp = "1.1"
      val pt = P.run(sql)(exp)
      pt should be(Right(Number(1.1)))
    }
    it("int is a Number expression") {
      val exp = "1"
      val pt = P.run(sql)(exp)
      pt should be(Right(Number(1)))
    }
  }
  describe("testing string expression") {
    it("string literal double quote is a StringLiteral expression") {
      val exp = "\"a string\""
      val pt = P.run(sql)(exp)
      pt should be(Right(StringLiteral("a string")))
    }
    it("string literal single quote is a StringLiteral expression") {
      val exp = "'a string'"
      val pt = P.run(sql)(exp)
      pt should be(Right(StringLiteral("a string")))
    }
  }

  describe("testing variable expression") {
    it("variable without namespace") {
      val exp = "varRef"
      val pt = P.run(sql)(exp)
      pt should be(Right(Variable(List(), "varRef")))
    }
    it("variable with one namespace") {
      val exp = "namespace.varRef"
      val pt = P.run(sql)(exp)
      pt should be(Right(Variable(List("namespace"), "varRef")))
    }
    it("variable with 2 namespace") {
      val exp = "namespace1.namespace2.varRef"
      val pt = P.run(sql)(exp)
      pt should be(Right(Variable(List("namespace1", "namespace2"), "varRef")))
    }
  }

  describe("function expression") {
    it("function without arguments") {
      val exp = "fn()"
      val pt = P.run(sql)(exp)
      pt should be(Right(Function("fn", List())))
    }
    it("function without arguments with spaces") {
      val exp = "fn ( )"
      val pt = P.run(sql)(exp)
      pt should equal(Right(Function("fn", List())))
    }
    it("function with one variable argument") {
      val exp = "fn(var1)"
      val pt = P.run(sql)(exp)
      pt should be(Right(Function("fn", List(Variable(List(), "var1")))))
    }
    it("function with 2 variable argument") {
      val exp = "fn ( var1, var2 )"
      val pt = P.run(sql)(exp)
      pt should be(Right(Function("fn", List(Variable(List(), "var1"), Variable(List(), "var2")))))
    }
    it("function with one * as variable argument") {
      val exp = "fn( *)"
      val pt = P.run(sql)(exp)
      pt should be(Right(Function("fn", List(Variable(List(), "*")))))
    }
    it("function with 2 * as variable argument should not parsed since * itself is a list of argument") {
      val exp = "fn(*, *)"
      val pt = P.run(sql)(exp)
      pt should be('left)
    }
  }

  describe("parenthesised expression") {
    it("(1) is a parenthesised expression") {
      val pt = P.run(sql)("(1)")
      pt should be(Right(Number(1)))
    }
    it("( 1 ) is a parenthesised expression") {
      val pt = P.run(sql)("( 1 )")
      pt should be(Right(Number(1)))
    }
  }

  describe("multiplication expression") {
    it("1 * 2 is a multiplication expression") {
      val pt = P.run(sql)("1+2")
      pt should be(Right(Op(Number(1),"+", Number(2))))
    }
    it("m * n is a multiplication expression") {
      val pt = P.run(sql)("m*n")
      pt should be(Right(Op(Variable(List(),"m"), "*",Variable(List(),"n"))))
    }
    it("m * n * v is a multiplication expression") {
      val pt = P.run(sql)("m*n*v")
      pt should be(Right(Op(Op(Variable(List(),"m"),"*",Variable(List(),"n")),"*",Variable(List(),"v"))))
    }
  }
}

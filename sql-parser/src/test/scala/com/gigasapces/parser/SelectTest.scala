package com.gigasapces.parser

import com.gigaspaces.parser.SQL._
import com.gigaspaces.parser.instances.Reference
import com.gigaspaces.parser.{SQL, SQLParser}
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers._

// https://alvinalexander.com/scala/scalatest-tutorials-from-scala-cookbook
// http://www.scalatest.org/at_a_glance/FlatSpec

class SelectTest extends FunSpec with BeforeAndAfter {
  import com.gigaspaces.parser.instances.ReferenceTypes.Parser
  val P: Reference.type = com.gigaspaces.parser.instances.Reference
  val sql: Parser[SQL] = SQLParser.sqlParser(P)

    describe("testing 'select * from table'") {
      it("should parse select * statement") {
        val statement = "select * from table"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(AllFields(),TableNames(List(TableName("table",None))))))
      }
      it("should parse select *' statement with table alias") {
        val statement = "select * from table1 foo"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(AllFields(),TableNames(List(TableName("table1",Some("foo")))))))
      }
      it("should parse select *' statement with many table alias") {
        val statement = "select * from table1 foo, table2 bar"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(AllFields(),TableNames(List(TableName("table1",Some("foo")), TableName("table2",Some("bar")))))) )
      }
      it("should parse 'select a' statement with selecting one field'") {
        val statement = "select a from table1 foo"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(SomeFields(List(Field(None,"a"))),TableNames(List(TableName("table1",Some("foo")))))))
      }
      it("should parse 'select foo.a' statement with selecting one field that has a namespace'") {
        val statement = "select foo.a from table1 foo"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(SomeFields(List(Field(Some("foo"),"a"))),TableNames(List(TableName("table1",Some("foo")))))))
      }
      it("should parse 'select foo.a, foo.bar from table1 foo' statement with selecting multiple fields that has a namespace'") {
        val statement = "select foo.a, foo.bar from table1 foo"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(SomeFields(List(Field(Some("foo"),"a"), Field(Some("foo"),"bar"))),TableNames(List(TableName("table1",Some("foo")))))))
      }
    }
}

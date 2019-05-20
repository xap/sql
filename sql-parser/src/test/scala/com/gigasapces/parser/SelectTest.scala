package com.gigasapces.parser

import com.gigaspaces.parser.SQL.{AllFields, Select, TableName, TableNames}
import com.gigaspaces.parser.{Reference, SQL, SQLParser}
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers._

// https://alvinalexander.com/scala/scalatest-tutorials-from-scala-cookbook
// http://www.scalatest.org/at_a_glance/FlatSpec

class SelectTest extends FunSpec with BeforeAndAfter {
  import com.gigaspaces.parser.ReferenceTypes.Parser
  val P: Reference.type = com.gigaspaces.parser.Reference
  val sql: Parser[SQL] = SQLParser.sqlParser(P)

    describe("testing [select * from table]") {
      it("should parse select * statement") {
        val statement = "select * from table"
        val pt = P.run(sql)(statement)
          pt should be (Right(Select(AllFields,TableNames(List(TableName("table",None))))))
      }
      it("should parse select * statement with table alias") {
        val statement = "select * from table1 foo"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(AllFields,TableNames(List(TableName("table1",Some("foo")))))))
      }
      it("should parse select * statement with many table alias") {
        val statement = "select * from table1 foo, table2 bar"
        val pt = P.run(sql)(statement)
        pt should be (Right(Select(AllFields,TableNames(List(TableName("table1",Some("foo")), TableName("table2",Some("bar")))))) )
      }
    }
}

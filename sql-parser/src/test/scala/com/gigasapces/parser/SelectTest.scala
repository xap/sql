package com.gigasapces.parser

import com.gigaspaces.parser.SQL.{AllFields, Select, SimpleTable}
import com.gigaspaces.parser.{Reference, SQL, SQLParser}
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers._

//https://alvinalexander.com/scala/scalatest-tutorials-from-scala-cookbook
// http://www.scalatest.org/at_a_glance/FlatSpec

class SelectTest extends FunSpec with BeforeAndAfter {
  import com.gigaspaces.parser.ReferenceTypes.Parser
  val P: Reference.type = com.gigaspaces.parser.Reference
  val sql: Parser[SQL] = SQLParser.sqlParser(P)

    describe("testing [select * from table]") {
      val statement = "select * from table"
      it("should parse select * statement") {
        val pt = P.run(sql)(statement)
//        pt should be ('left)
//        pt should be ('right)
          pt should be (Right(Select(AllFields,SimpleTable("table"))))
      }
    }
}

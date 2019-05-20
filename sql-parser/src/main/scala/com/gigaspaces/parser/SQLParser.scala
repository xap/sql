package com.gigaspaces.parser



import language.higherKinds
import language.implicitConversions


object SQLParser {

  def sqlParser[Parser[+ _]](P: Parsers[Parser]): Parser[SQL] = {
    import SQL._
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    // parse one table name and maybe its alias
    def table : Parser[TableName] = (name ** opt(whitespace *> name)) map {case (name, alias)=> TableName(name, alias)}
    // parse one or more table name (with optional aliases sperated by ","
    def tables : Parser[TableNames] = sep1(table, skipBoth(",", whitespace)) map TableNames

    def select: Parser[Select] = ("select" *> "*" *> "from" *> tables).map{
       Select(AllFields, _)
    }

    select
  }
}

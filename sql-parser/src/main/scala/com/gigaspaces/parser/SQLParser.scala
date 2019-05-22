package com.gigaspaces.parser



import language.higherKinds
import language.implicitConversions


object SQLParser {

  def sqlParser[Parser[+ _]](P: Parsers[Parser]): Parser[SQL] = {
    import SQL._
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    // parse one table name and maybe its alias
    def table : Parser[TableName] = (name ** opt(whitespace *> name)) map {case (name, alias) => TableName(name, alias)}
    // parse one or more table name (with optional aliases sperated by ","
    def tables : Parser[Tables] = sep1(table, skipBoth(",", whitespace)) map TableNames

    def select: Parser[Select] = (("select" *> fields) ** (whitespace *> "from" *> tables)).map { case (fields, tables) =>
       Select(fields , tables)
    }

    def field: Parser[Field] = (name ** opt("." *> name)) map {case (first, second) => second match {
      case Some(field) => Field(Some(first), field)
      case None => Field(None, first);
    }}

    def someFields: Parser[SomeFields] = sep1(field, ",").map(SomeFields)
    def allFields: Parser[AllFields] = P.string("*") map { _ => AllFields()}

//    def fields = allFields or someFields
    def fields: Parser[Fields] = allFields or someFields

    select
  }
}

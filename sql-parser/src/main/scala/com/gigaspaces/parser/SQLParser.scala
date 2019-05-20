package com.gigaspaces.parser


import com.gigaspaces.parser.JSON.JArray

import language.higherKinds
import language.implicitConversions


object SQLParser {

  def sqlParser[Parser[+ _]](P: Parsers[Parser]): Parser[SQL] = {
    import SQL._
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def select = ("select" *> "*" *> "from" *> name).map{
      name => Select(AllFields, SimpleTable(name))
    }

    select
  }
}

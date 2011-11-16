package com.github.kputnam.bee.parser

import com.github.kputnam.bee.nodes._

class ValueExpression extends scala.util.parsing.combinator.RegexParsers {

  def topLevel: Parser[QuotationNode] =
    rep1(node) ^^ (es => QuotationNode(es))

  def quotation: Parser[QuotationNode] =
    "[" ~> rep(node) <~ "]" ^^ (es => QuotationNode(es))

  def string: Parser[StringLit] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ (e => StringLit(e))

  def boolean: Parser[BooleanLit] =
    "true|false".r ^^ (e => BooleanLit("true".equals(e)))

  def numeric: Parser[NumericLit] =
    "[+-]?[0-9]+(?:\\.[0-9]*)?".r ^^ (e => NumericLit(BigDecimal(e)))

  def name: Parser[NameNode] =
    "[^\\s\\[\\]]+".r ^^ (e => NameNode(e))

  def node: Parser[AbstractNode] =
    quotation | string | boolean | numeric | name

}
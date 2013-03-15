package sscheme

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  lazy val symbol = {
    val symbols = "!#$%&|*+-/:<=>?@^_~".toSet
    elem("symbol", { symbols.contains(_) })
  }

  def apply(input: String): Double = parseAll(symbol, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object SScheme extends App {
  Parser(args.lift(0).getOrElse(""))
}

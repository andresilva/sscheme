package sscheme

import java.lang.Character
import scala.util.parsing.combinator.{JavaTokenParsers, Parsers, RegexParsers}

sealed trait LispVal extends Any
case class Atom(val a: String) extends AnyVal with LispVal
case class SList(val l: List[LispVal]) extends AnyVal with LispVal
case class DottedList(val l: List[LispVal], val d: LispVal) extends LispVal
case class Number(val n: Int) extends AnyVal with LispVal
case class SString(val s: String) extends AnyVal with LispVal
case class Bool(val b: Boolean) extends AnyVal with LispVal

trait SchemeTokenParsers extends JavaTokenParsers {
  def oneOf(chars: Char*)(kind: String): Parser[Elem] = {
    val charsSet = chars.toSet
    elem(kind, { charsSet.contains(_) })
  }

  def oneOf(chars: String)(kind: String): Parser[Elem] =
    oneOf(chars.toSeq: _*)(kind)

  lazy val digit = elem("digit", { c => c >= '0' && c <= '9' })

  lazy val space = oneOf(' ', '\t', '\n', '\r')("space")
  lazy val spaces = space*

  lazy val symbol = oneOf("!#$%&|*+-/:<=>?@^_~")("symbol")

  lazy val letter = elem("letter", Character.isLetter(_))
}

object SchemeParser extends SchemeTokenParsers {
  lazy val atom = (letter | symbol) ~ (letter | digit | symbol).* ^^ {
    case first ~ rest =>
      val atom = (first :: rest).mkString
      atom match {
        case "#t" => Bool(true)
        case "#f" => Bool(false)
        case _ => Atom(atom)
      }
  }

  lazy val string = stringLiteral ^^ { SString(_) }

  lazy val number = digit.* ^^ { c => Number(c.mkString.toInt) }

  lazy val expr = atom | string | number

  def apply(input: String): String = parseAll(expr, input) match {
    case Success(result, _) => result.toString
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object SScheme extends App {
  println(SchemeParser(args.lift(0).getOrElse("")))
}

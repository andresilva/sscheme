package sscheme

import java.lang.Character
import scala.util.parsing.combinator.{JavaTokenParsers, Parsers, RegexParsers}

sealed trait LispVal extends Any
case class Atom(val a: String) extends AnyVal with LispVal
case class SList(val l: List[LispVal]) extends AnyVal with LispVal {
  override def toString = s"SList([${l.mkString(", ")}])"
}
case class DottedList(val l: List[LispVal], val d: LispVal) extends LispVal {
  override def toString = s"DottedList([${l.mkString(", ")}], $d)"
}
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

  def digit = elem("digit", { c => c >= '0' && c <= '9' })

  def space = oneOf(' ', '\t', '\n', '\r')("space")
  def spaces = space+

  def symbol = oneOf("!#$%&|*+-/:<=>?@^_~")("symbol")

  def letter = elem("letter", Character.isLetter(_))
}

object SchemeParser extends SchemeTokenParsers {
  def atom = (letter | symbol) ~ (letter | digit | symbol).* ^^ {
    case first ~ rest =>
      val atom = (first :: rest).mkString
      atom match {
        case "#t" => Bool(true)
        case "#f" => Bool(false)
        case _ => Atom(atom)
      }
  }

  def string = stringLiteral ^^ { SString(_) }

  def number = digit.+ ^^ { c => Number(c.mkString.toInt) }

  def list = repsep(expr, spaces) ^^ { SList(_) }

  def dottedList = (expr <~ spaces).* ~ (elem('.') ~> spaces ~> expr) ^^ {
    case head ~ tail =>
      DottedList(head, tail)
  }

  def lists = elem('(') ~> (dottedList | list) <~ elem(')')

  def quoted: Parser[SList] = elem(''') ~> expr ^^ { expr => SList(List(Atom("quote"), expr)) }

  def expr: Parser[LispVal] = atom | string | number | quoted | lists

  def apply(input: String): String = parseAll(expr, input) match {
    case Success(result, _) => result.toString
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object SScheme extends App {
  val input = args.lift(0).getOrElse("")
  println(s"Input: $input")
  println(s"Result: ${SchemeParser(input)}")
}

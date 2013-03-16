package sscheme

import java.lang.Character
import scala.util.parsing.combinator.{JavaTokenParsers, Parsers, RegexParsers}

sealed trait LispVal extends Any
case class Atom(val a: String) extends AnyVal with LispVal {
  override def toString = a
}
case class SList(val l: List[LispVal]) extends AnyVal with LispVal {
  override def toString = l.mkString("(", " ", ")")
}
case class DottedList(val h: List[LispVal], val t: LispVal) extends LispVal {
  override def toString = s"(${h.mkString(" ")} . $t)"
}
case class Number(val n: Int) extends AnyVal with LispVal {
  override def toString = n.toString
}
case class SString(val s: String) extends AnyVal with LispVal {
  override def toString = s""""$s""""
}
case class Bool(val b: Boolean) extends AnyVal with LispVal {
  override def toString = if (b) "#t" else "#f"
}

object SchemePrimitives {
  def unboxNum(v: LispVal): Int = v match {
    case Number(n) => n
    case _ => 0
  }

  def numBinOp(op: (Int, Int) => Int)(vs: List[LispVal]) =
    Number(vs.map(unboxNum).reduceLeft(op))

  val primitives: Map[String, List[LispVal] => LispVal] = Map(
    "+" -> { numBinOp(_ + _) },
    "-" -> { numBinOp(_ - _) },
    "*" -> { numBinOp(_ * _) },
    "/" -> { numBinOp(_ / _) },
    "remainder" -> { numBinOp(_ % _) }
  )

  def apply(func: String, args: List[LispVal]): LispVal =
    primitives.get(func).map(_(args)).getOrElse(Bool(false))
}

object SchemeInterpreter {
  def apply(v: LispVal): LispVal = v match {
    case s: SString => s
    case n: Number => n
    case b: Bool => b
    case SList(List(Atom("quote"), v)) => v
    case SList(Atom(func) :: args) => SchemePrimitives(func, args.map(apply))
  }
}

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

  def string = stringLiteral ^^ { s => SString(s.replaceAll("\"", "")) }

  def number = digit.+ ^^ { c => Number(c.mkString.toInt) }

  def list = repsep(expr, spaces) ^^ { SList(_) }

  def dottedList = (expr <~ spaces).* ~ (elem('.') ~> spaces ~> expr) ^^ {
    case head ~ tail =>
      DottedList(head, tail)
  }

  def lists = elem('(') ~> (dottedList | list) <~ elem(')')

  def quoted: Parser[SList] = elem(''') ~> expr ^^ { expr => SList(List(Atom("quote"), expr)) }

  def expr: Parser[LispVal] = atom | string | number | quoted | lists

  def apply(input: String): LispVal = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object SScheme extends App {
  val input = args.lift(0).getOrElse("")
  println(s"Input: $input")
  println(s"Result: ${SchemeInterpreter(SchemeParser(input))}")
}

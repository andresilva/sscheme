package sscheme

object SScheme extends App {
  val input = args.lift(0).getOrElse("")
  println(s"Input: $input")
  println(s"Result: ${Interpreter(Parser(input))}")
}

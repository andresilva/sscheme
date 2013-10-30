package sscheme

object Repl extends App {
  def repl {
    try {
      val input = readPrompt
      if (input != "quit") {
        val result = Interpreter(Parser(input))
        println("res: " + result.toString)
        repl
      }
    } catch {
      case _: Exception => println("you broke the interpreter."); repl
    }
  }

  def readPrompt: String = { print("sscheme> "); readLine }

  repl
}

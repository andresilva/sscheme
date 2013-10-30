package sscheme

object Interpreter {
  def eval(v: LispVal): LispVal = v match {
    case s: SString => s
    case n: Number => n
    case b: Bool => b
    case SList(List(Atom("quote"), v)) => v
    case SList(List(Atom("if"), pred, conseq, alt)) => eval(pred) match {
      case Bool(true) => eval(conseq)
      case _ => eval(alt)
    }
    case SList(Atom(func) :: args) => Primitives(func, args.map(eval))
  }

  def apply(v: LispVal): LispVal = eval(v)
}

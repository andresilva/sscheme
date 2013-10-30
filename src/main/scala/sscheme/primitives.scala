package sscheme

object Primitives {
  def unboxNum(v: LispVal): Int = v match {
    case Number(n) => n
    case _ => 0
  }

  def unboxStr(v: LispVal): String = v match {
    case SString(s) => s
    case Number(n) => n.toString
    case Bool(b) => b.toString
    case _ => ""
  }

  def unboxBool(v: LispVal): Boolean = v match {
    case Bool(b) => b
    case _ => false
  }

  def boolBinOp[T](unboxer: LispVal => T)(op: (T, T) => Boolean)(vs: List[LispVal]) = {
    require(vs.length == 2)
    Bool(op(unboxer(vs(0)), unboxer(vs(1))))
  }

  def numBoolBinOp = boolBinOp(unboxNum) _
  def strBoolBinOp = boolBinOp(unboxStr) _
  def boolBoolBinOp = boolBinOp(unboxBool) _

  def numBinOp(op: (Int, Int) => Int)(vs: List[LispVal]) =
    Number(vs.map(unboxNum).reduceLeft(op))

  def unaryOp(op: LispVal => LispVal)(vs: List[LispVal]) = vs match {
    case List(v) => op(v)
  }

  object types {
    def isSymbol(v: LispVal) = v match {
      case _: Atom => Bool(true)
      case _ => Bool(false)
    }

    def isNumber(v: LispVal) = v match {
      case _: Number => Bool(true)
      case _ => Bool(false)
    }

    def isString(v: LispVal) = v match {
      case _: SString => Bool(true)
      case _ => Bool(false)
    }

    def isBool(v: LispVal) = v match {
      case _: Bool => Bool(true)
      case _ => Bool(false)
    }

    def isList(v: LispVal) = v match {
      case _: SList => Bool(true)
      case _: DottedList => Bool(true)
      case _ => Bool(false)
    }
  }

  object lists {
    def car(vs: List[LispVal]): LispVal = vs match {
      case List(SList(x :: xs)) => x
      case List(DottedList(x :: xs, _)) => x
      case List(x) => throw new RuntimeException("type mistmatch 'pair'")
      case _ => throw new RuntimeException("bad arg number. 1 required")
    }

    def cdr(vs: List[LispVal]): LispVal = vs match {
      case List(SList(x :: xs)) => SList(xs)
      case List(DottedList(Seq(_), x)) => x
      case List(DottedList(_ :: xs, x)) => DottedList(xs, x)
      case List(x) => throw new RuntimeException("type mistmatch 'pair'")
      case _ => throw new RuntimeException("bad arg number. 1 required")
    }

    def cons(vs: List[LispVal]): LispVal = vs match {
      case List(x, SList(Nil)) => SList(List(x))
      case List(x, SList(xs)) => SList(x :: xs)
      case List(x, DottedList(xs, xlast)) => DottedList(x :: xs, xlast)
      case List(x1, x2) => DottedList(List(x1), x2)
      case _ => throw new RuntimeException("bad arg number. 2 required")
    }

    def eqv(vs: List[LispVal]): LispVal = vs match {
      case List(Bool(arg1), Bool(arg2)) => Bool(arg1 == arg2)
      case List(Number(arg1), Number(arg2)) => Bool(arg1 == arg2)
      case List(SString(arg1), SString(arg2)) => Bool(arg1 == arg2)
      case List(Atom(arg1), Atom(arg2)) => Bool(arg1 == arg2)
      case List(DottedList(xs1, x1), DottedList(xs2, x2)) => eqv(List(SList(xs1 ++ List(x1)), SList(xs2 ++ List(x2))))
      case List(SList(arg1), SList(arg2)) => Bool(arg1.length == arg2.length && arg1.zip(arg2).forall(p => p._1 == p._2))
      case _ => throw new RuntimeException("bad arg number. 2 required")
    }
  }

  val primitives: Map[String, List[LispVal] => LispVal] = Map(
    "+" -> { numBinOp(_ + _) },
    "-" -> { numBinOp(_ - _) },
    "*" -> { numBinOp(_ * _) },
    "/" -> { numBinOp(_ / _) },
    "remainder" -> { numBinOp(_ % _) },
    "symbol?" -> { unaryOp(types.isSymbol) },
    "string?" -> { unaryOp(types.isString) },
    "number?" -> { unaryOp(types.isNumber) },
    "bool?" -> { unaryOp(types.isBool) },
    "list?" -> { unaryOp(types.isList) },
    "=" -> { numBoolBinOp(_ == _) },
    "<" -> { numBoolBinOp(_ < _) },
    ">" -> { numBoolBinOp(_ > _) },
    "/=" -> { numBoolBinOp(_ != _) },
    ">=" -> { numBoolBinOp(_ >= _) },
    "<=" -> { numBoolBinOp(_ <= _) },
    "&&" -> { boolBoolBinOp(_ && _) },
    "||" -> { boolBoolBinOp(_ || _) },
    "string=?" -> { strBoolBinOp(_ == _) },
    "string<?" -> { strBoolBinOp(_ < _) },
    "string>?" -> { strBoolBinOp(_ > _) },
    "string<=?" -> { strBoolBinOp(_ <= _) },
    "string>=?" -> { strBoolBinOp(_ >= _) },
    "car" -> lists.car,
    "cdr" -> lists.cdr,
    "cons" -> lists.cons,
    "eq?" -> lists.eqv,
    "eqv?" -> lists.eqv)

  def apply(func: String, args: List[LispVal]): LispVal =
    primitives.get(func).map(_(args)).getOrElse(Bool(false))
}

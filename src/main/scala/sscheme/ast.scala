package sscheme

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

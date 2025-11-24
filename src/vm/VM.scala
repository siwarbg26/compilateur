package vm

import generator.Ins
import generator.Ins.*
import vm.Value.*

import scala.annotation.tailrec

enum Value:
  case IntVal(n: Int)

type Env = List[Value]
case class VMState(a: Value, s: List[Value], e: Env, c: List[Ins])

object VM:
  def execute(c: List[Ins]): Value =
    execute(IntVal(0), List(), List(), c)

  @tailrec
  def execute(a: Value, s: List[Value], e: Env, c: List[Ins]): Value = (a, s, e, c) match
    case (_, _, _, Nil) => a
    case (_, _, _, Ldi(n)::c1) => execute(IntVal(n), s, e, c1)
    case (IntVal(n), IntVal(m)::s1, _, Add::c1) => execute(IntVal(m + n), s1, e, c1)
    case (IntVal(n), IntVal(m)::s1, _, Sub::c1) => execute(IntVal(m - n), s1, e, c1)
    case (IntVal(n), IntVal(m)::s1, _, Mul::c1) => execute(IntVal(m * n), s1, e, c1)
    case (IntVal(n), IntVal(0)::_, _, Div::_) => throw Exception("division by zero")
    case (IntVal(n), IntVal(m)::s1, _, Div::c1) => execute(IntVal(m / n), s1, e, c1)
    // JZ: si a == 0 -> continuer (exécuter z-branch placé juste après), sinon sauter offset instructions à partir de la suite
    case (IntVal(n), _, _, JZ(offset)::c1) =>
      if n == 0 then execute(a, s, e, c1)
      else
        val newC = c1.drop(offset) // ajuster si convention différente (offset vs offset-1)
        execute(a, s, e, newC)
    // JMP: sauter offset instructions inconditionnellement
    case (_, _, _, JMP(offset)::c1) =>
      val newC = c1.drop(offset) // ajuster si nécessaire
      execute(a, s, e, newC)
    case state => throw Exception(s"unexpected VM state $state")

@main
def test(): Unit =
  // petit test simple : calcule 1 + 2 => 3
  println(VM.execute(List(Ldi(1), Ldi(2), Add)))

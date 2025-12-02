package generator

import ast.Term
import ast.Term.*
import ast.Op
import Ins.*

type Code = List[Ins]

object Generator:
  def gen(term: Term): Code = term match
    case Number(value) => List(Ldi(value))

    case IfZero(cond, zBranch, nzBranch) =>
      val condCode = gen(cond)
      val thenCode = gen(zBranch)
      val elseCode = gen(nzBranch)
      condCode ++ List(Test(thenCode, elseCode))

    case BinaryTerm(op, u, v) =>
      val c_u = gen(u)
      val c_v = gen(v)
      c_u ++ List(Push) ++ c_v ++ List(gen_op(op))

    case _ =>
      throw new UnsupportedOperationException("Generator: construct non gérée")

  def gen_op(op: Op): Ins = op match
    case Op.Plus  => Add
    case Op.Minus => Sub
    case Op.Times => Mul
    case Op.Div   => Div

// scala
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
      val zCode = gen(zBranch)
      val nzCode = gen(nzBranch)
      // JZ prend un offset relatif pour sauter au code du zBranch
      // +2 pour passer l'instruction JZ elle-même et un éventuel JMP
      condCode ++ List(JZ(zCode.length + 2)) ++ zCode ++ List(JMP(nzCode.length + 1)) ++ nzCode
    case BinaryTerm(op, u, v) =>
      val c_u = gen(u)
      val c_v = gen(v)
      // évaluer u puis v puis appliquer l'opération
      c_u ++ c_v ++ List(gen_op(op))
    case _ =>
      throw new UnsupportedOperationException("Generator: construct non gérée (seulement constantes, binaires et IfZero pour le TP)")

  def gen_op(op: Op): Ins = op match
    case Op.Plus  => Add
    case Op.Minus => Sub
    case Op.Times => Mul
    case Op.Div   => Div

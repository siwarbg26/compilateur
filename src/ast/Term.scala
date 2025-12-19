package ast

import generator.ATerm
import generator.ATerm.{AApp, AFun}

enum Term:
  case Number(value: Int)
  case IfZero(cond: Term, zBranch: Term, nzBranch: Term)
  case BinaryTerm(op: Op, exp1: Term, exp2: Term)
  case Var(name: String)
  case Let(name: String, t1: Term, t2: Term)
  case Fun(param: String, body: Term)
  case App(func: Term, arg: Term)
  case Fix(name: String, body: Term)

  def annotate(env: List[String]): ATerm = this match
    case Number(n) =>
      ATerm.AInt(n)

    case Var(name) =>
      val index = env.indexOf(name)
      ATerm.AVar(name, index)

    case BinaryTerm(Op.Plus, left, right) =>
      ATerm.AAdd(left.annotate(env), right.annotate(env))

    case BinaryTerm(Op.Minus, left, right) =>
      ATerm.ASub(left.annotate(env), right.annotate(env))

    case BinaryTerm(Op.Times, left, right) =>
      ATerm.AMul(left.annotate(env), right.annotate(env))

    case BinaryTerm(Op.Div, left, right) =>
      ATerm.ADiv(left.annotate(env), right.annotate(env))

    case IfZero(cond, thenBranch, elseBranch) =>
      ATerm.AIf(cond.annotate(env), thenBranch.annotate(env), elseBranch.annotate(env))

    case Let(name, value, body) =>
      ATerm.ALet(name, value.annotate(env), body.annotate(name :: env))
    case Fun(param, body) =>
      AFun(param, body.annotate(param :: env))

    case App(func, arg) =>
      AApp(func.annotate(env), arg.annotate(env))
    case _ =>
      throw new UnsupportedOperationException("Cas non géré")



enum Op:
  case Plus
  case Minus
  case Times
  case Div

object Op:
  def parse(s: String): Op =
    s match
      case "+" => Plus
      case "-" => Minus
      case "*" => Times
      case "/" => Div
      case _   => throw new IllegalArgumentException(s"Opérateur non reconnu: $s")

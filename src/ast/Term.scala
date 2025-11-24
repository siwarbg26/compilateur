package ast

enum Term:
  case Number(value: Int)
  case IfZero(cond: Term, zBranch: Term, nzBranch: Term)
  case BinaryTerm(op: Op, exp1: Term, exp2: Term)
  case Var(name: String)
  case Let(name: String, t1: Term, t2: Term)
  case Fun(param: String, body: Term)
  case App(func: Term, arg: Term)
  case Fix(name: String, body: Term)

  def annotate(e: List[String]): ATerm = this

type ATerm = Term

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

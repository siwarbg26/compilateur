package generator

import ast.Term
import generator.ATerm

object Generator:
  def gen(term: Term): List[Ins] =
    val annotated = term.annotate(List())
    genA(annotated)

  private def genA(term: ATerm): List[Ins] = term match
    case ATerm.AInt(n) =>
      List(Ins.Ldi(n))

    case ATerm.AVar(_, index) =>
      List(Ins.Search(index))

    case ATerm.AAdd(left, right) =>
      genA(left) ++ List(Ins.Push) ++ genA(right) ++ List(Ins.Add)

    case ATerm.ASub(left, right) =>
      genA(left) ++ List(Ins.Push) ++ genA(right) ++ List(Ins.Sub)

    case ATerm.AMul(left, right) =>
      genA(left) ++ List(Ins.Push) ++ genA(right) ++ List(Ins.Mul)

    case ATerm.ADiv(left, right) =>
      genA(left) ++ List(Ins.Push) ++ genA(right) ++ List(Ins.Div)

    case ATerm.AIf(cond, thenBranch, elseBranch) =>
      genA(cond) ++ List(Ins.Test(genA(thenBranch), genA(elseBranch)))

    case ATerm.ALet(_, value, body) =>
      List(Ins.Pushenv) ++ genA(value) ++ List(Ins.Extend) ++ genA(body) ++ List(Ins.Popenv)

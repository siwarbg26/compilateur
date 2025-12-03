package generator

import generator.ATerm.*
import generator.Ins.*

object Generator:
  def genA(t: ATerm): List[Ins] = t match
    case AInt(n) =>
      List(Ldi(n))

    case AVar(name, index) =>
      List(Search(index))

    case AAdd(left, right) =>
      genA(left) ++ List(Push) ++ genA(right) ++ List(Add)

    case ASub(left, right) =>
      genA(left) ++ List(Push) ++ genA(right) ++ List(Sub)

    case AMul(left, right) =>
      genA(left) ++ List(Push) ++ genA(right) ++ List(Mul)

    case ADiv(left, right) =>
      genA(left) ++ List(Push) ++ genA(right) ++ List(Div)

    case AIf(cond, thenBranch, elseBranch) =>
      genA(cond) ++ List(Test(genA(thenBranch), genA(elseBranch)))

    case ALet(name, value, body) =>
      List(Pushenv) ++ genA(value) ++ List(Extend) ++ genA(body) ++ List(Popenv)

    case AFun(param, body) =>
      List(Mkclos(genA(body) ++ List(Return)))

    case AApp(func, arg) =>
      List(Pushenv) ++ genA(arg) ++ List(Push) ++ genA(func) ++ List(Apply, Popenv)

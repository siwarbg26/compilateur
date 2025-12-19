package generator

import generator.ATerm.*
import generator.Ins.*

object Generator:

  def gen(t: ATerm): List[Ins] =
    try
      TypeChecker.check(t)
      genA(t)
    catch
      case e: TypeError => throw new Exception(s"Type error: ${e.getMessage}")

  def genA(t: ATerm): List[Ins] = t match
    case AInt(n) =>
      List(Ldi(n))

    case AVar(name, index) =>
      List(Search(index))

    case AAdd(left, right) =>
      genA(left) ++ genA(right) ++ List(Add)

    case ASub(left, right) =>
      genA(left) ++ genA(right) ++ List(Sub)

    case AMul(left, right) =>
      genA(left) ++ genA(right) ++ List(Mul)

    case ADiv(left, right) =>
      genA(left) ++ genA(right) ++ List(Div)

    case AIf(cond, thenBranch, elseBranch) =>
      genA(cond) ++ List(Test(genA(thenBranch), genA(elseBranch)))

    case ALet(name, value, body) =>
      List(Pushenv) ++ genA(value) ++ List(Extend) ++ genA(body) ++ List(Popenv)

    case AFun(param, body) =>
      List(Mkclos(genA(body) ++ List(Return)))

    case AApp(func, arg) =>
      genA(arg) ++ List(Push) ++ genA(func) ++ List(Apply)

    case AFix(name, body) =>
      body match
        case AFun(_, inner) => List(Fix(genA(inner) ++ List(Return)))
        case _ => List(Fix(genA(body)))

  type Code = List[Ins]

  def genAM(t: ATerm): Code = genA(t)

  type CodeWAT = List[WAT]

  enum WAT:
    case Ins(ins: String)
    case Test(code1: CodeWAT, code2: CodeWAT)

  def genWAT(code: Code): String =
    s"""(module (func (export "main") (result i32)
       |${format(1, emit(code))}
       | return))""".stripMargin

  private def emit(code: Code): CodeWAT =
    code.flatMap(emitIns)

  private def emitIns(ins: Ins): CodeWAT = ins match
    case Ldi(n) => List(WAT.Ins(s"i32.const $n"))
    case Add => List(WAT.Ins("i32.add"))
    case Sub => List(WAT.Ins("i32.sub"))
    case Mul => List(WAT.Ins("i32.mul"))
    case Div => List(WAT.Ins("i32.div_s"))
    case Test(thenCode, elseCode) =>
      List(WAT.Test(emit(thenCode), emit(elseCode)))
  // Autres instructions (Search, Extend, etc.) non traitées pour l'instant, car non requises pour PCF vert

  private def format(depth: Int, code: CodeWAT): String =
    code.map(formatIns(depth, _)).mkString("\n")

  private def formatIns(depth: Int, ins: WAT): String =
    ins match
      case WAT.Ins(s) => spaces(depth) + s
      case WAT.Test(c1, c2) =>
        val thenStr = format(depth + 1, c1)
        val elseStr = format(depth + 1, c2)
        s"${spaces(depth)}if (result i32)\n$thenStr\n${spaces(depth)}else\n$elseStr\n${spaces(depth)}end"

  private def spaces(depth: Int): String = "  " * depth

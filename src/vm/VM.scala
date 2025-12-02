package vm

import generator.Ins
import generator.Ins.*

enum Value:
  case IntVal(n: Int)

object VM:
  def execute(code: List[Ins]): Value =
    execute(List(), List(), List(), List(), code)

  private def execute(a: List[Value], s: List[Value], env_stack: List[List[Value]], e: List[Value], c: List[Ins]): Value =
    (a, s, env_stack, e, c) match
      case (v::_, _, _, _, Nil) => v

      case (_, _, _, _, Ldi(n)::c1) =>
        execute(Value.IntVal(n)::a, s, env_stack, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, _, Add::c1) =>
        execute(Value.IntVal(v1 + v2)::a1, s, env_stack, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, _, Sub::c1) =>
        execute(Value.IntVal(v1 - v2)::a1, s, env_stack, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, _, Mul::c1) =>
        execute(Value.IntVal(v1 * v2)::a1, s, env_stack, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, _, Div::c1) =>
        execute(Value.IntVal(v1 / v2)::a1, s, env_stack, e, c1)

      case (v::a1, _, _, _, Push::c1) =>
        execute(v::a1, v::s, env_stack, e, c1)

      case (Value.IntVal(n)::a1, _, _, _, Test(thenCode, elseCode)::c1) =>
        if n == 0 then
          execute(a1, s, env_stack, e, thenCode ++ c1)
        else
          execute(a1, s, env_stack, e, elseCode ++ c1)

      case (_, _, _, _, Search(index)::c1) =>
        val value = e(index)
        execute(value::a, s, env_stack, e, c1)

      case (v::a1, _, _, _, Extend::c1) =>
        execute(a1, s, env_stack, v::e, c1)

      case (_, _, _, _, Pushenv::c1) =>
        execute(a, s, e::env_stack, e, c1)

      case (_, _, saved_env::es1, _, Popenv::c1) =>
        execute(a, s, es1, saved_env, c1)

      case _ =>
        throw new RuntimeException(s"unexpected VM state")

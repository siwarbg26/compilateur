package vm

import generator.Ins
import generator.Ins.*

enum Value:
  case IntVal(n: Int)
  case Closure(code: List[Ins], env: List[Value])

object VM:
  def execute(code: List[Ins]): Value =
    execute(List(), List(), List(), List(), code)

  private def execute(
                       a: List[Value],
                       s: List[Value],
                       env_stack: List[(List[Value], List[Ins])],
                       e: List[Value],
                       c: List[Ins]
                     ): Value =
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
        execute(a, s, (e, Nil)::env_stack, e, c1)

      case (_, _, (savedEnv, _)::envStack1, _, Popenv::c1) =>
        execute(a, s, envStack1, savedEnv, c1)

      case (_, _, _, _, Mkclos(code)::c1) =>
        execute(Value.Closure(code, e)::a, s, env_stack, e, c1)

      case (Value.Closure(code, closure_env)::arg::a1, _, _, _, Apply::c1) =>
        execute(a1, s, (e, c1)::env_stack, arg::closure_env, code)

      case (v::a1, _, (saved_env, saved_code)::envStack1, _, Return::_) =>
        execute(v::a1, s, envStack1, saved_env, saved_code)

      case _ =>
        throw new RuntimeException(s"unexpected VM state")

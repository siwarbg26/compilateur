package vm

import generator.Ins
import generator.Ins.*

enum Value:
  case IntVal(n: Int)

object VM:
  def execute(code: List[Ins]): Value =
    execute(List(), List(), List(), code)

  private def execute(a: List[Value], s: List[Value], e: List[Value], c: List[Ins]): Value =
    (a, s, e, c) match
      // Fin d'exécution - retourner la valeur en haut de la pile
      case (v::_, _, _, Nil) => v

      // Charger une constante
      case (_, _, _, Ldi(n)::c1) =>
        execute(Value.IntVal(n)::a, s, e, c1)

      // Opérations binaires - CORRECTION ICI
      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Add::c1) =>
        execute(Value.IntVal(v1 + v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Sub::c1) =>
        execute(Value.IntVal(v1 - v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Mul::c1) =>
        execute(Value.IntVal(v1 * v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Div::c1) =>
        execute(Value.IntVal(v1 / v2)::a1, s, e, c1)

      // Saut conditionnel
      case (Value.IntVal(n)::a1, _, _, JZ(offset)::c1) =>
        if n == 0 then
          // Si n == 0, on continue (exécute le then-branch)
          execute(a1, s, e, c1)
        else
          // Si n != 0, on saute (va au else-branch)
          execute(a1, s, e, c1.drop(offset))

      // Saut inconditionnel
      case (_, _, _, JMP(offset)::c1) =>
        execute(a, s, e, c1.drop(offset))

      // Cas d'erreur
      case _ =>
        throw new RuntimeException(s"unexpected VM state")

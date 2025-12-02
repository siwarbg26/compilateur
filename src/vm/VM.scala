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

      // Opérations binaires
      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Add::c1) =>
        execute(Value.IntVal(v1 + v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Sub::c1) =>
        execute(Value.IntVal(v1 - v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Mul::c1) =>
        execute(Value.IntVal(v1 * v2)::a1, s, e, c1)

      case (Value.IntVal(v2)::Value.IntVal(v1)::a1, _, _, Div::c1) =>
        execute(Value.IntVal(v1 / v2)::a1, s, e, c1)

      // Push - sauvegarder la valeur courante sur la pile SANS la retirer de l'accumulateur
      case (v::a1, _, _, Push::c1) =>
       execute(v::a1, v::s, e, c1)

      // Test - branchement conditionnel
      case (Value.IntVal(n)::a1, _, _, Test(thenCode, elseCode)::c1) =>
        if n == 0 then
          execute(a1, s, e, thenCode ++ c1)
        else
          execute(a1, s, e, elseCode ++ c1)

      // Cas d'erreur
      case _ =>
        throw new RuntimeException(s"unexpected VM state")

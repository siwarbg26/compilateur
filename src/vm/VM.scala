package vm

import generator.Ins
import generator.Ins.*

enum Value:
  case IntVal(n: Int)
  case Closure(code: List[Ins], env: () => List[Value])

object VM:

  // CallFrame sauvegarde aussi la scopeStack du caller
  private case class CallFrame(
                                savedEnv: List[Value],
                                savedCode: List[Ins],
                                savedS: List[Value],
                                savedA: List[Value],
                                savedScope: List[List[Value]]
                              )

  def execute(code: List[Ins]): Value =
    execute(List(), List(), List(), List(), List(), code)

  // a: accu, s: pile d'arguments, callStack: pile d'appels,
  // scopeStack: pile de scopes, e: env courant, c: code courant
  private def execute(
                       a: List[Value],
                       s: List[Value],
                       callStack: List[CallFrame],
                       scopeStack: List[List[Value]],
                       e: List[Value],
                       c: List[Ins]
                     ): Value =
    (a, s, callStack, scopeStack, e, c) match
      // fin normale : résultat en tête de l'accu quand plus d'instructions
      case (v :: _, _, _, _, _, Nil) => v

      case (Nil, _, _, _, _, Nil) =>
        throw new RuntimeException("unexpected VM state: no result on accumulator")

      case (_, _, _, _, _, Ldi(n) :: c1) =>
        execute(Value.IntVal(n) :: a, s, callStack, scopeStack, e, c1)

      case (Value.IntVal(v2) :: Value.IntVal(v1) :: a1, _, _, _, _, Add :: c1) =>
        execute(Value.IntVal(v1 + v2) :: a1, s, callStack, scopeStack, e, c1)

      case (Value.IntVal(v2) :: Value.IntVal(v1) :: a1, _, _, _, _, Sub :: c1) =>
        execute(Value.IntVal(v1 - v2) :: a1, s, callStack, scopeStack, e, c1)

      case (Value.IntVal(v2) :: Value.IntVal(v1) :: a1, _, _, _, _, Mul :: c1) =>
        execute(Value.IntVal(v1 * v2) :: a1, s, callStack, scopeStack, e, c1)

      case (Value.IntVal(v2) :: Value.IntVal(v1) :: a1, _, _, _, _, Div :: c1) =>
        execute(Value.IntVal(v1 / v2) :: a1, s, callStack, scopeStack, e, c1)

      case (v :: a1, _, _, _, _, Push :: c1) =>
        // duplique la valeur courante sur la pile d'arguments
        execute(v :: a1, v :: s, callStack, scopeStack, e, c1)

      case (Value.IntVal(n) :: a1, _, _, _, _, Test(thenCode, elseCode) :: c1) =>
        if n == 0 then execute(a1, s, callStack, scopeStack, e, thenCode ++ c1)
        else execute(a1, s, callStack, scopeStack, e, elseCode ++ c1)

      case (_, _, _, _, _, Search(index) :: c1) =>
        if index < 0 || index >= e.length then
          throw new RuntimeException(s"Search: index hors limites $index (env=$e)")
        val value = e(index)
        execute(value :: a, s, callStack, scopeStack, e, c1)

      case (v :: a1, _, _, _, _, Extend :: c1) =>
        // lie la valeur au début de l'environnement courant
        execute(a1, s, callStack, scopeStack, v :: e, c1)

      case (_, _, _, _, _, Pushenv :: c1) =>
        // cadre de portée : empiler l'environnement courant
        execute(a, s, callStack, e :: scopeStack, e, c1)

      case (_, _, _, savedScope :: scopeTail, _, Popenv :: c1) =>
        // restaure l'environnement sauvegardé par le dernier Pushenv
        execute(a, s, callStack, scopeTail, savedScope, c1)

      case (_, _, Nil, Nil, _, Popenv :: _) =>
        // pas de cadre de portée à restaurer
        throw new RuntimeException("Popenv: no matching Pushenv (scopeStack empty)")

      case (_, _, _, _, _, Mkclos(code) :: c1) =>
        // crée une closure capturant l'environnement courant
        execute(Value.Closure(code, () => e) :: a, s, callStack, scopeStack, e, c1)

      case (_, _, _, _, _, Fix(code) :: c1) =>
        // crée une closure récursive avec auto-référence
        lazy val cl: Value.Closure = Value.Closure(code, () => cl :: e)
        execute(cl :: a, s, callStack, scopeStack, e, c1)

      // APPLY : closure en tête de l'accu, argument sur la pile `s`
      case ((cl @ Value.Closure(code, envFun)) :: aTail, arg :: sTail, callStack, scopeStack, eCur, Apply :: c1) =>
        // sauvegarder l'accu du caller sans la closure (aTail) ; aInit est l'accu pour exécuter la closure
        val closure_env = envFun()
        val savedA = aTail.tail // nettoyer en retirant l'argument laissé sur a
        val aInit = aTail.tail                 // pas de Extend pour Fix
        // sauvegarder aussi la scopeStack du caller
        val callFrame = CallFrame(eCur, c1, sTail, savedA, scopeStack)
        // nouvel environnement de la closure : l'argument en tête suivi de son env capturé
        execute(aInit, sTail, callFrame :: callStack, scopeStack, arg :: closure_env, code)

      // RETURN : restaurer le dernier CallFrame et placer le résultat sur l'accu du caller
      case (v :: _, _, callFrame :: callTail, _, _, Return :: _) =>
        val CallFrame(savedEnv, savedCode, savedS, savedA, savedScope) = callFrame
        val vValue = v.asInstanceOf[Value]
        val callTailTyped = callTail.asInstanceOf[List[CallFrame]]
        execute(vValue :: savedA, savedS, callTailTyped, savedScope, savedEnv, savedCode)

      case _ =>
        throw new RuntimeException(s"unexpected VM state: a=$a s=$s callStack=$callStack scopeStack=$scopeStack e=$e c=$c")


package generator

import generator.ATerm.*

import scala.collection.mutable

object TypeChecker:

  private var nextId = 0
  private def freshVar(): TVar =
    val id = nextId
    nextId += 1
    TVar(id)

  def check(t: ATerm): GType =
    nextId = 0
    val ty = typeOf(t, List())
    prune(ty)

  // pruning (follow instances)
  private def prune(t: GType): GType = t match
    case tv @ TVar(_, Some(inst)) =>
      val p = prune(inst)
      tv.instance = Some(p)
      p
    case other => other

  // free type variables of a type
  private def ftv(t: GType): Set[Int] = prune(t) match
    case TVar(id, None) => Set(id)
    case TVar(_, Some(inst)) => ftv(inst)
    case TFun(a, b) => ftv(a) ++ ftv(b)
    case TInt => Set()

  // free type variables of a scheme (vars are quantified)
  private def ftvScheme(s: Scheme): Set[Int] =
    ftv(s.t) -- s.vars.toSet

  // free vars in environment
  private def ftvEnv(env: List[Scheme]): Set[Int] =
    env.foldLeft(Set.empty[Int])((acc, s) => acc ++ ftvScheme(s))

  // généraliser : variables libres dans t mais pas dans env deviennent quantifiées
  private def generalize(t: GType, env: List[Scheme]): Scheme =
    val vars = ftv(t) -- ftvEnv(env)
    Scheme(vars.toList, t)

  // instancier un schéma : remplacer vars généralisées par de nouveaux TVar
  private def instantiate(s: Scheme): GType =
    val mapping = mutable.Map.empty[Int, TVar]
    def inst(t: GType): GType = prune(t) match
      case TVar(id, _) if s.vars.contains(id) =>
        mapping.getOrElseUpdate(id, freshVar())
      case TVar(_, _) => prune(t)
      case TFun(a, b) => TFun(inst(a), inst(b))
      case TInt => TInt
    inst(s.t)

  // occurs check
  private def occurs(v: TVar, t: GType): Boolean =
    prune(t) match
      case tv: TVar => tv eq v
      case TFun(a, b) => occurs(v, a) || occurs(v, b)
      case _ => false

  private def unify(a: GType, b: GType): Unit =
    val ta = prune(a)
    val tb = prune(b)
    (ta, tb) match
      case (x: TVar, y: TVar) if x eq y => ()
      case (x: TVar, y) =>
        if occurs(x, y) then throw TypeError(s"Occurs check failed: ${show(x)} in ${show(y)}")
        x.instance = Some(y)
      case (x, y: TVar) =>
        if occurs(y, x) then throw TypeError(s"Occurs check failed: ${show(y)} in ${show(x)}")
        y.instance = Some(x)
      case (TInt, TInt) => ()
      case (TFun(a1, b1), TFun(a2, b2)) =>
        unify(a1, a2)
        unify(b1, b2)
      case _ =>
        throw TypeError(s"Types incompatibles: ${show(ta)} vs ${show(tb)}")

  private def typeOf(t: ATerm, env: List[Scheme]): GType = t match
    case AInt(_) => TInt

    case AVar(_, index) =>
      if index < 0 || index >= env.length then
        throw TypeError(s"Variable non liée (index = $index)")
      else
        instantiate(env(index))

    case AAdd(l, r) =>
      val lt = typeOf(l, env); val rt = typeOf(r, env)
      unify(lt, TInt); unify(rt, TInt); TInt

    case ASub(l, r) =>
      val lt = typeOf(l, env); val rt = typeOf(r, env)
      unify(lt, TInt); unify(rt, TInt); TInt

    case AMul(l, r) =>
      val lt = typeOf(l, env); val rt = typeOf(r, env)
      unify(lt, TInt); unify(rt, TInt); TInt

    case ADiv(l, r) =>
      val lt = typeOf(l, env); val rt = typeOf(r, env)
      unify(lt, TInt); unify(rt, TInt); TInt

    case AIf(cond, thenB, elseB) =>
      val ct = typeOf(cond, env); unify(ct, TInt)
      val tt = typeOf(thenB, env); val et = typeOf(elseB, env)
      unify(tt, et)
      tt

    case ALet(_, value, body) =>
      val vt = typeOf(value, env)
      val scheme = generalize(vt, env)
      typeOf(body, scheme :: env)

    case AFun(_, body) =>
      val paramT = freshVar()
      val resT = typeOf(body, Scheme(Nil, paramT) :: env)
      TFun(paramT, resT)

    case AApp(func, arg) =>
      val fT = typeOf(func, env)
      val aT = typeOf(arg, env)
      val res = freshVar()
      unify(fT, TFun(aT, res))
      res

    case AFix(name, body) =>
      val tv = freshVar()
      val schemeForName = Scheme(Nil, tv)
      val bodyType = typeOf(body, schemeForName :: env)
      unify(tv, bodyType)
      prune(tv)

    case other =>
      throw TypeError(s"TypeChecker: cas non géré pour le terme: ${other}")

  private def show(t: GType): String = prune(t) match
    case TInt => "TInt"
    case TFun(a, b) => s"TFun(${show(a)},${show(b)})"
    case tv @ TVar(id, _) =>
      tv.instance match
        case None => s"TVar($id)"
        case Some(_) => show(prune(tv))

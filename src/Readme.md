# Strategy de typage (README)

Résumé
- Implémentation : Hindley‑Milner (HM) avec :
    - types : `TInt`, `TFun(arg,res)`, `TVar(id, instance)`
    - schémas `Scheme(vars, t)` pour généralisation
    - opérations : `prune`, `occurs`, `unify`, `generalize`, `instantiate`
- But : supporter fonctions de première classe et let‑polymorphisme afin d'accepter des programmes comme `red18` / `red19`.

Pourquoi cela corrige `red18` / `red19`
- Ancienne stratégie : hypothèse pragmatique que les paramètres de `fun` étaient `Int`, entraînant des unifications impossibles quand une fonction était passée en argument.
- Avec HM + `Scheme` : un `let` générique (par ex. `id = fun x -> x`) est généralisé en `forall a. a -> a`. Lorsqu'on utilise `id` dans des contextes différents, on instancie un type frais, évitant la collision `TInt vs TFun(...)`. Ainsi `red18` / `red19` doivent maintenant réussir.

Reproduction / debug rapide
1. Lancer les tests : `sbt "runMain test.TestInterp"` ou utiliser la classe `test/TestInterp.scala`.
2. Pour inspecter l'ATerm généré : ajouter un `println(aterm)` après `val aterm = term.annotate(List())` dans `PCF.compile`.
3. Pour tracer l'inférence : activer le mode debug (voir patch ci‑dessous) afin que le `TypeChecker` imprime chaque sous-terme et son type inféré.

Que faire si on obtient encore une erreur (ex. `red11`)
- Signification de l'erreur observée : `Types incompatibles: TInt vs TFun(TVar(...),TVar(...))`
    - Cela signifie qu'un sous-terme (par exemple une branche d'un `ifz` ou un argument d'opération arithmétique) produit un type fonctionnel là où un `Int` est attendu.
- Causes probables :
    - Le programme source est réellement mal typé (ex. `ifz cond then 1 else (fun x -> x)`).
    - Ou une mauvaise annotation/erreur d'indexation (une variable référencée renvoie la mauvaise entrée de l'environnement et devient une fonction).
    - Ou un cas `Fix` / récursion mal traité (mais si `Fix` n'est pas annoté correctement, on aurait une autre exception).
- Actions recommandées :
    1. Ouvrir `test/compilateur/test/red11.pcf` et vérifier le code source : est‑ce que l'une des branches du `ifz` renvoie une fonction ?
    2. Lancer la compilation avec les prints d'ATerm et/ou l'inférence en mode verbose pour localiser le sous‑terme produisant `TFun(...)`.
    3. Si l'erreur vient d'une indexation (AVar index incorrect), imprimer les `AVar(name,index)` dans l'ATerm pour repérer les `index` négatifs ou mal positionnés.
    4. Si le code est correct mais le TypeChecker donne un type fonctionnel par erreur, fournir `red11.pcf` pour diagnostic précis.

Patch de debug (optionnel)
- Ajoute des prints temporaires dans `TypeChecker.check` / `typeOf` pour afficher chaque sous‑terme et son type inféré. Utiliser uniquement pour debug local.

Exemple de patch minimal (ajouter des prints) :
```scala
// scala
// Dans compilauteur/src/generator/TypeChecker.scala :
// ajouter un paramètre verbose à check, ou une var debug = true pour debug local.
// Extrait minimal à insérer dans typeOf pour tracer :

// au début de l'objet TypeChecker
private var debug = false
def enableDebug(): Unit = debug = true

// dans typeOf, après le calcul du type pour un sous-terme t (juste avant de retourner)
if debug then
  println(s"TypeChecker: terme = ${t}, type = ${show(prune(res))}")

// exemple d'utilisation depuis PCF.compile avant d'appeler TypeChecker.check :
TypeChecker.enableDebug()
TypeChecker.check(aterm)

# Résumé : ajout de Type et TypeChecker pour regler les erreurs de type de red18 et red 19 

But
- Vérifier les types avant génération de code pour éviter les erreurs d'exécution liées à des incohérences de types (ex. `TInt` vs `TFun(...)`).
- Supporter le let‑polymorphisme (généralisation/instanciation) afin que des fonctions comme `id = fun x -> x` soient correctement réutilisables.

Fichiers ajoutés
- `compilateur/src/generator/Type.scala`
    - Définit les représentations de types utilisées par l'inférence :
        - `TInt`, `TFun(arg,res)`, `TVar(id, instance)`
    - Définit `Scheme(vars, t)` pour la généralisation (let‑polymorphisme).
    - Définit `TypeError` pour signaler les erreurs de typage.

- `compilateur/src/generator/TypeChecker.scala`
    - Implémente l'inférence Hindley‑Milner appliquée aux `ATerm`.
    - Fonctionnalités principales :
        - génération de variables de type fraîches (`TVar`) ;
        - `prune` pour suivre les instances et normaliser les `TVar` ;
        - `occurs` et `unify` (avec occurs‑check) pour unification sûre ;
        - gestion des `Scheme` : `generalize` au niveau de `let` et `instantiate` à l'utilisation ;
        - parcours des constructions `ATerm` (constantes, variables indexées, opérations arithmétiques, `if`, `let`, `fun`, `app`) et levée de `TypeError` en cas d'incohérence.
    - Utilisé avant la génération (appelé depuis `Generator.gen`) : lève une `TypeError` si le programme n'est pas typable.

Rôle pratique
- Empêche la génération d'instructions pour des programmes mal typés.
- Corrige les cas où une même définition doit être polymorphe (évite les erreurs du type "attendu `TInt`, trouvé `TFun(...)`").

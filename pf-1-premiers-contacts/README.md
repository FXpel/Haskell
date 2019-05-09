#   Premiers contacts

Ce dépôt correspond aux deux premières séances de TP de PF.


##  Instructions pour rendre votre travail avec gitlab

Pour permettre à votre chargé de TD de suivre votre travail sur ce projet :

-   *forkez* ce dépôt (bouton _Fork_),
-   dans le dépôt *forké*, ajoutez votre chargé de TD aux membres du
    projet avec l’accès _Developer_,
-   éditez ce fichier pour indiquez vos noms (membres du binôme) et
    supprimer ce paragraphe d’instructions.


##  Contenu du dépôt

Il contient les fichiers suivants :

-   `dragon.hs` est le canevas pour le dessin de la courbe du Dragon,
-   `Setup.hs`, `premiers-contacts.cabal`, `stack.yaml` sont des
    fichiers de configuration pour
    [Cabal](https://www.haskell.org/cabal/) et
    [Stack](https://docs.haskellstack.org/en/stable/README/).

Ces fichiers ne sont réellement utiles que si vous voulez utiliser ces
outils pour installer les bibliothèques nécessaires sur une machine
personnelle.


##  Utilisation de `stack` sur votre machine personnelle.

Cette section n’est pas un substitut à la documentation de l’outil.
Elle vous donne juste une commande qui peut aider à démarrer.

Si vous voulez installer la bibliothèque Gloss nécessaire pour
dessiner la courbe du Dragon en utilisant l’outil `stack` (que vous
devrez avoir installé, bien entendu), vous pourrez utiliser :

```console
stack --install-ghc build
```

Cette commande devrait éventuellement installer GHC, Gloss, et autres
dépendances puis compiler `dragon.hs` en un exécutable créé quelque
part dans le répertoire `.stack-work/install` (le chemin dépend de
votre configuration).

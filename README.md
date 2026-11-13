# Décomposition de séries spatio-temporelles - évolution des décès en Belgique

Ce projet nécessite d'avoir assimilé l'ensemble des notions des modules quatre et cinq du cours de science des données biologiques 3 relatifs à l'analyse de séries spatio-temporelles. Il correspond au template GitHub <https://github.com/BioDataScience-Course/C05Ia_tsd>. Il est distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

## Objectifs

Ce projet est **individuel**, **court** et **cadré**. Vous devrez :

-   Créer un objet `ts` à partir des données brutes
-   Décomposer une série spatio-temporelle
-   Visualiser et décrire les composantes de votre série spatio-temporelle

## Présentation des données

L'office des statistiques de Belgique répertorie le nombre de décès en Belgique au cours du temps. Ces données sont disponibles ici : <https://statbel.fgov.be/fr/themes/population/mortalite-et-esperance-de-vie/tables-de-mortalite-et-esperance-de-vie>. Vous avez à votre disposition le nombre de décès par mois.

## Consignes

Vous allez mettre en pratique vos compétences en matière de décomposition de séries spatio-temporelles. Complétez le fichier `death_notebook.qmd` en fonction des indications qui s'y trouvent.

Lorsque vous aurez terminé de compléter les fichiers, compilez le document final HTML (bouton **Rendu**) et assurez-vous que cette compilation se fait sans erreurs. Dans le cas contraire, apportez les corrections nécessaires.

Vous avez une batterie de tests à votre disposition (onglet "Construire" -\> bouton "Construire tout") pour vérifier l'état de votre travail. Note : utilisez ces tests à la fin. Concentrez-vous d'abord sur vos analyses, puis faites un rendu final du document lorsque tout est complété, et seulement après, utilisez les tests. Il est contre-productif d'activer les tests à chaque nouvelle ligne de code ajoutée dans votre document ! **Afin d'éviter tout abus d'utilisation des tests, vous ne pouvez les utiliser que dix fois dans ce projet.**

Lors de votre commit-pull-push final, assurez-vous de ne plus avoir d'erreur de compilation en fichier HTML. Cela fait partie de l'exercice et les erreurs restant à ce niveau devront être lourdement sanctionnées. Vérifiez que votre dernier commit a bien été pushé sur GitHub avant la fin de la séance.

## Évaluation : un document qui compile, sur GitHub

**Un projet dont le document ne compile pas à partir de la version présente sur GitHub obtient la note de zéro pour ce travail.** Un document qui compile sur votre ordinateur, mais pas depuis GitHub (fichier oublié dans le commit, push non effectué...), est considéré comme ne compilant pas.

## Utilisation de l’IA

Dans le cadre de votre travail, vous avez le droit d’être aidé par l’intelligence artificielle. Le chatbot SciViews est disponible dans votre RStudio sur Saturn Cloud via l’addin Help. Il répond aux questions concernant le langage R, les statistiques et la science des données.

Vous restez responsables de tout ce que contiennent vos documents : vérifiez le code et les interprétations proposés par l'IA, et assurez-vous de pouvoir les expliquer.

L'utilisation de l'IA doit être déclarée dans la sous-section **Déclaration sur l'utilisation de l'IA** de la partie Matériel et méthodes de `death_notebook.qmd` : cochez la case qui correspond à votre situation dans le chunk `ai_comment` (remplacez `[]` par `[X]`), puis précisez en dessous l'utilisation que vous avez faite de l'IA et citez le ou les modèles employés.

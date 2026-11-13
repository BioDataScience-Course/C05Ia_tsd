# Vérifications de death_notebook.qmd
death <- try(parsermd::parse_qmd("../../death_notebook.qmd"))
expect_true(TRUE, info = death)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("death_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.
  
  expect_true(is_rendered_current("death_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction", "But", "Matériel et méthodes",
    "Analyse", "Description de la série",
    "Stationnarisation et étude des cycles", "Décomposition par LOESS",
    "Cycle saisonnier", "Étude des résidus", "Discussion et conclusions")
    %in% (rmd_node_sections(death) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).
  
  expect_true(all(c("setup", "ai_comment", "import", "dts", "dtscomment", "dacf",
    "dacfcomment", "dtrend", "dtrendcomment", "dtsdiff", "dspectrum",
    "dspectrumcomment", "dloess", "dloesscomment", "tseries", "sspectrum",
    "sspectrumcomment", "month", "monthcomment", "racf", "racfcomment",
    "discussioncomment")
    %in% rmd_node_label(death)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).
  
  expect_false(any(duplicated(na.omit(rmd_node_label(death)))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  authors <- death[[1]]@yaml[["author"]]
  expect_true(authors != "___")
  expect_true(!grepl("__", authors))
  expect_true(grepl("^[^_]....+", authors))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.
  
  expect_true(grepl("[a-z]", authors))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
  
  expect_true(grepl("[A-Z]", authors))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import' : importation des données brutes", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `death` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # n'avez pas bien rempli le code du chunk 'import'.
  
  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `death` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.
  
  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `death` est incorrect
  # Vérifiez que vous importez bien le fichier `data/death_month.rds` avec
  # `read()`, sans filtrer les lignes.
})

test_that("Chunks 'dts', 'dtscomment' : conversion en objet ts et visualisation", {
  expect_true(is_identical_to_ref("dts"))
  # La série temporelle obtenue dans le chunk 'dts' n'est pas celle attendue 
  # Vérifiez les paramètres de `ts()`. La série doit commencer en 1992. Quelle
  # est l'unité de temps à choisir ? Combien d'observations avons-nous par unité
  # de temps (frequence =) ? Vous ne devez pas convertir tout le tableau `death`
  # mais uniquement la colonne qui contient les mortalités pour obtenir un objet
  # **ts** (série temporelle univarié).
  
  expect_true(is_identical_to_ref("dtscomment"))
  # L'interprétation du graphique de la série `death_ts` est (partiellement) 
  # fausse dans le chunk 'dtscomment'
  # Vous devez cocher les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'dacf' & 'dacfcomment' : Analyse de l'autocorrélation", {
  expect_true(is_identical_to_ref("dacf"))
  # Le graphique de l'autocorrélation n'est pas réalisé ou est incorrect
  # Vérifiez sur base des instructions vos calculs. Avez-vous bien assigné le
  # résultat à l'objet 'dacf' ? Avez-vous bien utilisé la fonction 'acf()' ?
  
  expect_true(is_identical_to_ref("dacfcomment"))
  # L'interprétation du graphique d'autocorrélation est (partiellement) fausse
  # Vous devez cocher les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'dtrend' & 'dtrendcomment' : Test de tendance générale par bootstrap", {
  expect_true(is_identical_to_ref("dtrend"))
  # Le test de tendance générale par bootstrap n'est pas réalisé ou est
  # incorrect
  # Avez-vous bien employé une valeur de R de 999 ?
  
  expect_true(is_identical_to_ref("dtrendcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cocher les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'dtsdiff', 'dspectrum' & 'dspectrumcomment' : Cycles de la série stationnarisée", {
  expect_true(is_identical_to_ref("dtsdiff"))
  # La stationnarisation de la série `death_ts` n'est pas réalisée ou est
  # incorrecte
  # Avez-vous bien choisi un décalage (lag) de moitié de la fréquence de la
  # série ?
  
  expect_true(is_identical_to_ref("dspectrum"))
  # Le graphique n'est pas réalisé ou est incorrect.
  # Avez-vous extrait la composante "filtered" ?
  # Avez-vous bien employé les valeurs de 3 et de 5 pour lisser le spectre ?
  # Avez-vous effectué le périodogramme de la série stationnarisée 'death_stat' ?
  # Avez-vous bien assigné le résultat du calcul à `dspec` ?
  
  expect_true(is_identical_to_ref("dspectrumcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cocher la phrase qui décrit le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'dloess', 'dloesscomment' & 'tseries' : Décomposition LOESS de la série", {
  expect_true(is_identical_to_ref("dloess"))
  # La décomposition LOESS de la série `death_ts` n'est pas réalisée ou est
  # incorrecte
  # Vous devez indiquer que vous voulez extraire la tendance.
  # Vous devez indiquer que vous voulez extraire un cycle saisonnier par moyenne
  # mobile en choisissant une fenêtre `s.window=` impaire immédiatement
  # supérieure à la fréquence de la série.

  expect_true(is_identical_to_ref("dloesscomment"))
  # L'interprétation de la décomposition LOESS est (partiellement) fausse
  # Vous devez cocher la phrase qui décrit le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
    
  expect_true(is_identical_to_ref("tseries"))
  # L'extraction des composantes de la décomposition LOESS en un objet **mts**
  # n'est pas faite ou est incorrecte.
  # Vous devez utiliser ici la fonction `tseries` et assigner le résultat à
  # `death_mts`.
})

test_that("Chunks 'sspectrum' & 'sspectrumcomment' : Cycles de la composante saisonnière", {
  expect_true(is_identical_to_ref("sspectrum"))
  # Le graphique n'est pas réalisé ou est incorrect.
  # Avez-vous réalisé l'analyse spectrale de la composante "seasonal" ?
  # Avez-vous bien employé les valeurs de 3 et de 5 pour lisser le spectre ?
  # Avez-vous effectué le périodogramme d'une composante de la série
  # `death_mts` ?
  # Avez-vous bien assigné le résultat du calcul à `sspec` ?
  
  expect_true(is_identical_to_ref("sspectrumcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cocher la phrase qui décrit le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'month' & 'monthcomment' : Mortalité par mois", {
  expect_true(is_identical_to_ref("month"))
  # Le graphique n'est pas réalisé ou est incorrect.
  # Avez-vous bien employé la composante "seasonal" de `death_mts` ?
  # Avez-vous bien assigné le résultat à 'bpl' ?
  
  expect_true(is_identical_to_ref("monthcomment"))
  # L'interprétation du graphique est (partiellement) fausse
  # Vous devez cocher les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'racf' & 'racfcomment' : Analyse de l'autocorrélation des résidus", {
  expect_true(is_identical_to_ref("racf"))
  # Le graphique de l'autocorrélation des résidus n'est pas réalisé ou est
  # incorrect
  # Vérifiez sur base des instructions vos calculs. Avez-vous bien assigné le
  # résultat à l'objet 'racf' ? Avez-vous bien utilisé la fonction 'acf()' ?
  
  expect_true(is_identical_to_ref("racfcomment"))
  # L'interprétation du graphique d'autocorrélation des résidus est
  # (partiellement) fausse
  # Vous devez cocher les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunk 'discussioncomment' : discussion et conclusions", {
  expect_true(is_identical_to_ref("discussioncomment"))
  # La discussion et les conclusions sont (partiellement) fausses
  # Vous devez cocher les phrases qui forment une discussion et des conclusions
  # correctes d'un 'x' entre les crochets [] -> [x]. Ensuite, vous devez
  # recompiler la version HTML du bloc-notes (bouton 'Rendu') sans erreur pour
  # réactualiser les résultats.
  # Reprenez l'ensemble de vos analyses (tendance générale, cycle saisonnier,
  # résidus) et rappelez-vous de ce qui s'est passé en 2020 et 2021.
})

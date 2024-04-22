# Vérifications de oats_notebook.qmd
oats <- parse_rmd("../../oats_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes oats_notebook est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("oats_notebook.qmd"))
  # La version compilée HTML du carnet de notes oats_notebook est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.
  
  expect_true(is_rendered_current("oats_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document oats_notebook est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Étude descriptive", "ANOVA", "Tests post-hoc",
    "Analyse des résidus du modèle", "Discussion et conclusions",
    "Références") %in% (rmd_node_sections(oats) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes oats_notebook ne sont pas
  # toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).
  
  expect_true(all(c("setup", "oatsimport", "oatscomment", "oatsplot",
    "oatsplotcomment", "oatsanova", "oatsanovacomment", "oatsconfint",
    "oatsconfintcomment", "oatsposthoc", "oatsposthoccomment", "oatsqqplot",
    "oatsqqplotcomment", "oatsresid",
    "oatsresidcomment") %in% rmd_node_label(oats)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent dans
  # oats_notebook.qmd
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).
  
  expect_true(any(duplicated(rmd_node_label(oats))))
  # Un ou plusieurs labels de chunks sont dupliqués dans oats_notebook.qmd
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans oats_ca ?", {
  expect_true(oats[[1]]$author != "___")
  expect_true(!grepl("__", oats[[1]]$author))
  expect_true(grepl("^[^_]....+", oats[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.
  
  expect_true(grepl("[a-z]", oats[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
  
  expect_true(grepl("[A-Z]", oats[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'oatsimport', 'oatscomment' : importation des données `oats`", {
  expect_true(is_identical_to_ref("oatsimport", "names"))
  # Les colonnes dans le tableau `oats` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct.
  
  expect_true(is_identical_to_ref("oatsimport", "classes"))
  # La nature des variables (classe) dans le tableau `oats` est incorrecte
  # Vérifiez le chunk d'importation des données `oatsimport`.
  
  expect_true(is_identical_to_ref("oatsimport", "nrow"))
  # Le nombre de lignes dans le tableau `oats` est incorrect
  # Vérifiez le chunk d'importation des données `oatsimport`.
  
  expect_true(is_identical_to_ref("oatscomment"))
  # Les commentaires sur les données sont(partiellement) faux dans le chunk
  # 'oatscomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsplot', 'oatsplotcomment' : graphique des données", {
  expect_true(is_identical_to_ref("oatsplot"))
  # Le graphique produit par le chunk 'oatsplot' n'est pas celui attendu.
  # Vérifiez le graphique réalisé qui doit être un nuage de points avec les
  # blocs en facettes (six graphiques assemblés en une grille 3x2 avec un
  # graphique par bloc).
  
  expect_true(is_identical_to_ref("oatsplotcomment"))
  # Les commentaires sur le graphique à facettes sont(partiellement) faux dans
  # le chunk 'oatsplotcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsanova', 'oatsanovacomment' : ANOVA de type split-plot", {
  expect_true(is_equal_to_ref("oatsanova"))
  # Le modèle ANOVA split-plot dans le chunk 'oatsanova' n'est pas celui attendu
  # Vérifiez votre code, et en particulier la formule qui spécifie le modèle
  # dans le chunk 'oatsanova'. Déterminez quel est le facteur fixe et le facteur
  # aléatoire ici et ne les intervertissez pas dans la formule. Vous considérez
  # dans ce modèle que l'effet des parcelles est le même quelle que soit la
  # concentration en azote de l'engrais.
  # Notez bien que le test peut échouer même lorsque votre modèle est correct
  # car il y a plusieurs façons d'écrire la formule et l'objet obtenu est
  # complexe.
  
  expect_true(is_identical_to_ref("oatsanovacomment"))
  # L'interprétation de l'ANOVA split-plot est (partiellement) fausse dans le
  # chunk 'oatsanovacomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsconfint', 'oatsconfintcomment' : Intervalles de confiance des paramètres de l'ANOVA split-plot", {
  expect_true(is_equal_to_ref("oatsconfint"))
  # Le calcul des intervalles de confiance des paramètres de votre ANOVA
  # split-plot dans le chunk 'oatsconfint' n'est pas celui attendu
  # Vérifiez votre code dans le chunk 'oatsconfint'.
  
  expect_true(is_identical_to_ref("oatsconfintcomment"))
  # L'interprétation des intervalles de confiance de l'ANOVA split-plot est
  # (partiellement) fausse dans le chunk 'oatsconfintcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsposthoc', 'oatsposthoccomment' : Comparaisons multiples pour l'azote", {
  expect_true(is_equal_to_ref("oatsposthoc"))
  # L'analyse post hoc ou le graphique est incorrect dans le chunk 'oatsposthoc'
  # Vérifiez votre code dans ce chunk.
  
  expect_true(is_identical_to_ref("oatsposthoccomment"))
  # L'interprétation des tests post hoc pour l'azote est (partiellement) fausse
  # dans le chunk 'oatsposthoccomment'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsqqplot', 'oatsqqplotcomment' : vérification de la normalité des résidus", {
  expect_true(is_identical_to_ref("oatsplot"))
  # Le graphique quantile-quantile produit par le chunk 'oatsqqplot' n'est pas
  # celui attendu.
  # Vérifiez le graphique réalisé dans ce chunk.
  
  expect_true(is_identical_to_ref("oatsqqplotcomment"))
  # Les commentaires sur le graphique quantile-quantile sont (partiellement)
  # faux dans le chunk 'oatsqqlotcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'oatsresid', 'oatsresidcomment' : analyse des résidus du modèle", {
  expect_true(is_identical_to_ref("oatsresid"))
  # Le graphique d'analyse des résidus produit par le chunk 'oatsresid' n'est
  # pas celui attendu.
  # Vérifiez le graphique réalisé dans ce chunk.
  
  expect_true(is_identical_to_ref("oatsresidcomment"))
  # Les commentaires sur le graphique d'analyse des résidus sont (partiellement)
  # faux dans le chunk 'oatsresidcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(oats, by_section("Discussion et conclusions")) |>
      as_document() |> grepl("...Votre discussion ici...", x = _,
        fixed = TRUE) |> any()))
  # La discussion et les conclusions ne sont pas faites
  # Remplacez "...Votre discussion ici..." par vos phrases de commentaires
  # libres (à noter que le contenu de cette section n'est pas évalué
  # automatiquement, mais il le sera par vos enseignants).
})

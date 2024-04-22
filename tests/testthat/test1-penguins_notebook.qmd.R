# Vérifications de penguins_notebook.qmd
penguins <- parse_rmd("../../penguins_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes penguins_notebook est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("penguins_notebook.qmd"))
  # La version compilée HTML du carnet de notes penguins_notebook est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.
  
  expect_true(is_rendered_current("penguins_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document penguins_notebook est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Analyse descriptive", "ANOVA", "Tests post-hoc",
    "Discussion et conclusions", "Références")
    %in% (rmd_node_sections(penguins) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes penguins_notebook ne sont pas
  # toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).
  
  expect_true(all(c("setup", "pengimport", "pengcomment", "pengrework",
    "pengtab", "pengtabcomment", "pengplot", "pengplotcomment",
    "penginteraction", "penginteractioncomment", "penganova1",
    "penganova1comment", "pengbartlett1", "pengresid1", "pengresidcomment",
    "penganova2", "penganova2comment", "pengposthoc2",
    "pengposthoc2comment") %in% rmd_node_label(penguins)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent dans
  # penguins_notebook.qmd
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).
  
  expect_true(any(duplicated(rmd_node_label(penguins))))
  # Un ou plusieurs labels de chunks sont dupliqués dans penguins_notebook.qmd
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans penguins_ca ?", {
  expect_true(penguins[[1]]$author != "___")
  expect_true(!grepl("__", penguins[[1]]$author))
  expect_true(grepl("^[^_]....+", penguins[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.
  
  expect_true(grepl("[a-z]", penguins[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
  
  expect_true(grepl("[A-Z]", penguins[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'pengimport', 'pengcomment' : importation et description des données `penguins`", {
  expect_true(is_identical_to_ref("pengimport", "names"))
  # Les colonnes dans le tableau `penguins` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct.
  
  expect_true(is_identical_to_ref("pengimport", "classes"))
  # La nature des variables (classe) dans le tableau `penguins` est incorrecte
  # Vérifiez le chunk d'importation des données `pengimport`.
  
  expect_true(is_identical_to_ref("pengimport", "nrow"))
  # Le nombre de lignes dans le tableau `penguins` est incorrect
  # Vérifiez le chunk d'importation des données `pengimport`.
  
  expect_true(is_identical_to_ref("pengcomment"))
  # Les commentaires sur les données sont(partiellement) fausse dans le chunk
  # 'pengcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'pengrework', 'pengtab', 'pengtabcomment' : élimination NA et tableau résumé", {
  expect_true(is_identical_to_ref("pengrework", "names"))
  # Les colonnes dans le tableau `peng` avec valeurs manquantes éliminées ne
  # sont pas celles attendues
  # Votre jeu de données `peng` n'est pas correct.
  
  expect_true(is_identical_to_ref("pengrework", "classes"))
  # La nature des variables (classe) dans le tableau `peng` est incorrecte
  # Vérifiez le chunk `pengrework`.
  
  expect_true(is_identical_to_ref("pengrework", "nrow"))
  # Le nombre de lignes dans le tableau `peng` est incorrect
  # Vérifiez le chunk `pengrework`. Avez-vous bien éliminé les lignes contenant
  # des valeurs manquantes ? Uniquement pour les variables cibles ?
  
  expect_true(is_identical_to_ref("pengtab", "names"))
  # Les colonnes dans le tableau `peng_tab` ne sont pas celles attendues
  # Votre tableau n'est pas correct.
  
  expect_true(is_identical_to_ref("pengtab", "classes"))
  # La nature des variables (classe) dans le tableau `peng_tab` est incorrecte
  # Vérifiez le chunk `pengtab`.
  
  expect_true(is_identical_to_ref("pengtab", "nrow"))
  # Le nombre de lignes dans le tableau `peng_tab` est incorrect
  # Vérifiez le chunk `pengtab`.
  
  expect_true(is_identical_to_ref("pengtabcomment"))
  # Les commentaires sur le tableau résumé sont(partiellement) faux dans le
  # chunk 'pengtabcomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'pengplot', 'pengplotcomment' : graphique des données de `peng`", {
  expect_true(is_identical_to_ref("pengplot"))
  # Le graphique produit par le chunk 'pengplot' n'est pas celui attendu.
  # Vérifiez le graphique réalisé qui doit être des boites de dispersion
  # parallèles avec un nuage de points "jitter" superposé et des points rouges
  # doivent, en outre, marquer les moyennes de chaque groupe.

  expect_true(is_identical_to_ref("pengplotcomment"))
  # Les commentaires sur le graphique en boites de dispersion parallèles sont 
  # (partiellement) fausse dans le chunk 'pengplotcomment'
  # Vous devez cochez les phrases qui décrivent le graphique d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'penginteraction', 'penginteractioncomment' : graphique des interactions de `peng`", {
  expect_true(is_identical_to_ref("penginteraction"))
  # Le graphique produit par le chunk 'penginteraction' n'est pas celui attendu.
  # Vérifiez votre graphique. Deux versions sont possibles (espèce en axe X et
  # sexe en couleur, ou sexe en axe X et espèce couleur). Retenez celui qui
  # montre le mieux les interactions.
  
  expect_true(is_identical_to_ref("penginteractioncomment"))
  # Les commentaires sur le graphique des interactions sont  (partiellement)
  # faux dans le chunk 'penginteractioncomment'
  # Vous devez cochez les phrases qui décrivent le graphique d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'penganova1', 'penganova1comment' : ANOVA espèce et sexe", {
  expect_true(is_equal_to_ref("penganova1"))
  # Le test dans le chunk 'penganova1' n'est pas celui attendu.
  # Vérifiez votre code et en particulier la formule utilisée dans le chunk
  # 'penganova1' pour l'objet `peng_lm`.
  
  expect_true(is_identical_to_ref("penganova1comment"))
  # L'interprétation de l'ANOVA est (partiellement) fausse dans le chunk
  # 'penganova1comment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'pengbartlett1', 'pengresid1', 'pengresidcomment' : Vérification des conditions d'application de l'ANOVA", {
  expect_true(is_equal_to_ref("pengbartlett1"))
  # Votre test d'homoscédasticité est incorrect dans le chunk 'pengbartlett1'
  # Vous devez effectuer un test de Bartlett avec les interactions entre espèce
  # et sexe (variable composite).
  
  expect_true(is_identical_to_ref("pengresid1"))
  # Le graphique est incorrect dans le chunk 'pengresid1'
  # Vous devez réaliser un graphique quantile-quantile des résidus de votre
  # ANOVA.
  
  expect_true(is_identical_to_ref("pengresidcomment"))
  # L'interprétation du test de Bartlett et du graphique quantile-quantile est
  # (partiellement) fausse dans le chunk 'pengresidcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'penganova2', 'penganova2comment' : ANOVA sur variable composite `species_sexe`", {
  expect_true(is_equal_to_ref("penganova2"))
  # L'ANOVA dans le chunk 'penganova2' n'est pas celle attendue.
  # Vérifiez votre code et en particulier la formule utilisée dans le chunk
  # 'penganova2' pour l'objet `peng_lm2`.
  
  expect_true(is_identical_to_ref("penganova2comment"))
  # L'interprétation de l'ANOVA 2 est (partiellement) fausse dans le chunk
  # 'penganova2comment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'pengposthoc2', 'pengposthoc2comment' : Comparaisons multiples sur variable composite `species_sex`", {
  expect_true(is_equal_to_ref("pengposthoc2"))
  # L'analyse post hoc ou le graphique est incorrect dans le chunk 'pengposthoc2'
  # Vérifiez votre code dans ce chunk. Vous devez utiliser la variable
  # `species_sex` ici.
  
  expect_true(is_identical_to_ref("pengposthoc2comment"))
  # L'interprétation des tests post hoc sur variable composite `species_sex` est
  # (partiellement) fausse dans le chunk 'pengposthoc2comment'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(penguins, by_section("Discussion et conclusions")) |>
      as_document() |> grepl("...Votre discussion ici...", x = _,
        fixed = TRUE) |> any()))
  # La discussion et les conclusions ne sont pas faites
  # Remplacez "...Votre discussion ici..." par vos phrases de commentaires
  # libres (à noter que le contenu de cette section n'est pas évalué
  # automatiquement, mais il le sera par vos enseignants).
})

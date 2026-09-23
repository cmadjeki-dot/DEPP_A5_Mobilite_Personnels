# Stratégie de tests

Les contrôles sont répartis en quatre familles complémentaires.

| Famille | Objet | Exemples |
|---|---|---|
| Tests logiciels | Comportement déterministe des fonctions et erreurs attendues | import, nettoyage, agrégations, pipeline |
| Contrôles qualité | Intégrité structurelle des données | clés, types, doublons, valeurs manquantes, fichiers |
| Contrôles statistiques | Validité numérique des résultats | bornes des proportions, dénominateurs, intervalles, métriques |
| Contrôles métier | Respect du champ et des limites d'interprétation | nomenclatures, transitions, secret statistique, absence de trajectoire individuelle |

Une anomalie détectée doit provoquer un échec explicite ou un statut documenté. Aucun
test ne corrige les données. La suite complète s'exécute depuis la racine avec
`testthat::test_dir("tests/testthat")`.

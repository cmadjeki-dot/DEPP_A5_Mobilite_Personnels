# DEPP A5 — Mobilite des personnels enseignants

Demonstrateur professionnel de statistique publique consacre a l'analyse des
parcours et des mobilites des personnels enseignants.

## Question generale

Quels facteurs individuels, professionnels, territoriaux et organisationnels
sont associes aux mobilites des personnels enseignants, et comment analyser
leurs trajectoires professionnelles dans le temps ?

## Principes

- utiliser en priorite des donnees publiques officielles ;
- documenter les sources, leur champ et leurs limites ;
- distinguer description, association, prediction et causalite ;
- ne jamais presenter une simulation comme un resultat observe ;
- garantir la reproductibilite avec `renv`, `targets`, `testthat` et Quarto ;
- ne jamais modifier les donnees brutes.

## Etat du projet

L'etape 0 initialise l'environnement technique. Aucune donnee ni aucun resultat
statistique ne sont encore integres. La recherche et l'audit des sources seront
realises dans une etape ulterieure.

## Prerequis

- R 4.6.1 ;
- Quarto 1.10.18 ;
- Git ;
- RStudio ou VS Code avec l'extension R.

## Reproduire l'environnement

Depuis la racine du projet :

```r
renv::restore()
targets::tar_make()
testthat::test_dir("tests/testthat")
```

Pour produire le document de controle :

```powershell
quarto render reports/initialisation.qmd
```

## Organisation

- `config/` : configuration et chemins relatifs ;
- `data/` : donnees brutes, externes, intermediaires et traitees non versionnees ;
- `R/` : fonctions reutilisables par domaine ;
- `reports/` : sources Quarto ;
- `dashboard/` : application interactive ;
- `outputs/` : figures, tables, modeles et journaux generes ;
- `tests/testthat/` : tests automatises ;
- `docs/` : documents rendus et informations de session.

## Pipeline

Le pipeline est defini dans `_targets.R`. Sa premiere cible,
`environment_manifest`, verifie le nom du projet, la version de R et
l'activation de `renv`.

## Confidentialite et versionnement

Les donnees, secrets, bibliotheques locales et sorties generees sont exclus par
`.gitignore`. Les donnees administratives confidentielles ne doivent jamais etre
deposees dans ce repertoire Git.

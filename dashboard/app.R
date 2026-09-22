library(shiny)
library(bslib)
library(ggplot2)

project_root <- here::here()
tables_root <- file.path(project_root, "outputs", "tables")
figures_root <- file.path(project_root, "outputs", "figures")

read_prepared <- function(...) {
  path <- file.path(tables_root, ...)
  if (!file.exists(path)) stop("Artefact prepare manquant: ", path, ". Executer le pipeline targets avant le dashboard.", call. = FALSE)
  utils::read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

# Chargement unique des resultats prepares: aucune analyse n'est recalculee ici.
prepared <- list(
  effectifs = read_prepared("descriptive", "effectifs.csv"),
  age = read_prepared("descriptive", "age.csv"),
  anciennete = read_prepared("descriptive", "anciennete.csv"),
  mobilite = read_prepared("descriptive", "mobilite.csv"),
  territoires = list(
    departement = read_prepared("territorial", "indicateurs_departements_2025.csv"),
    academie = read_prepared("territorial", "indicateurs_academies_2025.csv"),
    region = read_prepared("territorial", "indicateurs_regions_2025.csv")),
  trajectoires = read_prepared("trajectories", "profils_sequences_presence.csv"),
  coefficients = read_prepared("econometrics", "coefficients_logit.csv"),
  audit_ml = read_prepared("modeling", "audit_faisabilite.csv"),
  audit_survie = read_prepared("survival", "audit_faisabilite.csv"),
  evaluation = read_prepared("evaluation", "statut_evaluation.csv"),
  explicabilite = read_prepared("explainability", "registre_sorties.csv"),
  qualite = read_prepared("diagnostic_qualite.csv"))

shiny::addResourcePath("prepared-figures", figures_root)

app_theme <- bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#234E70", secondary = "#4E8F69")

source_note <- function(text) tags$p(class = "text-muted small", text)

ui <- bslib::page_navbar(
  title = "Observatoire des parcours et mobilités des personnels",
  theme = app_theme,
  fillable = TRUE,
  header = tags$div(class = "container-fluid bg-light border-bottom py-2",
    tags$span(class = "fw-semibold", "Données préparées par le pipeline reproductible — aucune analyse recalculée au lancement")),

  bslib::nav_panel("Vue générale",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        selectInput("general_year", "Année", choices = sort(unique(prepared$effectifs$annee)),
          selected = max(prepared$effectifs$annee)),
        selectInput("general_degree", "Degré", choices = unique(prepared$effectifs$degre))),
      bslib::layout_columns(
        bslib::value_box(title = "ETP enseignants", value = textOutput("total_etp"), showcase = "ETP"),
        bslib::value_box(title = "Établissements", value = textOutput("total_establishments"), showcase = "UAI"),
        bslib::value_box(title = "Variation annuelle", value = textOutput("annual_change"), showcase = "%")),
      bslib::card(full_screen = TRUE, bslib::card_header("Évolution des effectifs"), plotOutput("effectifs_plot", height = 420)),
      source_note("Source : DEPP, indicateurs de personnels agrégés par établissement. Unité : ETP et UAI."))),

  bslib::nav_panel("Profils",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        selectInput("profile_dimension", "Dimension", choices = c("Âge" = "age", "Ancienneté établissement" = "anciennete")),
        selectInput("profile_year", "Année", choices = sort(unique(c(prepared$age$annee, prepared$anciennete$annee))),
          selected = max(c(prepared$age$annee, prepared$anciennete$annee))),
        selectInput("profile_degree", "Degré", choices = unique(c(prepared$age$degre, prepared$anciennete$degre)))),
      bslib::card(full_screen = TRUE, bslib::card_header("Composition des ETP"), plotOutput("profile_plot", height = 480)),
      source_note("Les proportions sont calculées sur les ETP dont la composante est renseignée. L'ancienneté concerne l'établissement, pas la carrière."))),

  bslib::nav_panel("Mobilités",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        selectInput("mobility_indicator", "Indicateur départemental", choices = unique(prepared$mobilite$indicateur))),
      bslib::card(full_screen = TRUE, bslib::card_header("Distribution départementale — mouvement 1D public 2019"),
        plotOutput("mobility_plot", height = 450)),
      bslib::card(bslib::card_header("Départements"), tableOutput("mobility_table")),
      source_note("Ces ratios agrégés ne sont ni des probabilités individuelles ni un suivi longitudinal des personnels."))),

  bslib::nav_panel("Territoires",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        selectInput("geo_level", "Niveau géographique", choices = c("Département" = "departement", "Académie" = "academie", "Région" = "region")),
        selectInput("territorial_indicator", "Indicateur", choices = c(
          "Part âgée de 50 ans ou plus" = "part_age_50_plus",
          "Part avec moins de 2 ans d'ancienneté" = "part_anciennete_moins_2",
          "Part avec 8 ans ou plus d'ancienneté" = "part_anciennete_8_plus",
          "Part de femmes" = "part_femmes")),
        selectInput("territorial_map", "Carte départementale préparée", choices = c(
          "Âge 50 ans ou plus" = "age_50_plus", "Ancienneté inférieure à 2 ans" = "anciennete_moins_2",
          "Éducation prioritaire" = "education_prioritaire"))),
      bslib::layout_columns(
        bslib::card(full_screen = TRUE, bslib::card_header("Classement territorial"), plotOutput("territorial_plot", height = 540)),
        bslib::card(full_screen = TRUE, bslib::card_header("Carte officielle préparée"), uiOutput("territorial_map_image"))),
      source_note("Année : 2025. Les taux sont reconstruits par somme des numérateurs et dénominateurs; les cartes utilisent les contours officiels disponibles."))),

  bslib::nav_panel("Trajectoires",
    bslib::layout_columns(
      bslib::card(full_screen = TRUE, bslib::card_header("Séquences de présence des UAI"), plotOutput("trajectory_plot", height = 470)),
      bslib::card(bslib::card_header("Profils préparés"), tableOutput("trajectory_table"))),
    tags$div(class = "alert alert-warning mt-3",
      "Une apparition ou disparition du fichier n'est pas une entrée, une sortie ou une mobilité individuelle de personnel.")),

  bslib::nav_panel("Modélisation",
    bslib::accordion(
      bslib::accordion_panel("Économétrie associative", tableOutput("econometric_table")),
      bslib::accordion_panel("Machine Learning", tableOutput("ml_table")),
      bslib::accordion_panel("Analyse de survie", tableOutput("survival_table")),
      bslib::accordion_panel("Évaluation", tableOutput("evaluation_table")),
      bslib::accordion_panel("Explicabilité", tableOutput("explainability_table"))),
    tags$div(class = "alert alert-info mt-3",
      "Importance prédictive ≠ association statistique ≠ causalité. Aucun modèle individuel de mobilité n'est estimé avec l'open data.")),

  bslib::nav_panel("Méthodologie",
    bslib::layout_columns(
      bslib::card(bslib::card_header("Principes"),
        tags$ul(tags$li("Données raw préservées"), tags$li("Pipeline targets et environnement renv"),
          tags$li("Agrégations pondérées par les dénominateurs disponibles"), tags$li("Aucune causalité revendiquée"),
          tags$li("Aucune mobilité individuelle reconstruite"))),
      bslib::card(bslib::card_header("Diagnostic qualité préparé"), tableOutput("quality_table"))),
    source_note("Sources principales : DEPP, data.education.gouv.fr et Annuaire de l'éducation. Les limites détaillées figurent dans les notebooks du projet."))
)

server <- function(input, output, session) {
  selected_effectif <- reactive({
    subset(prepared$effectifs, annee == input$general_year & degre == input$general_degree)
  })
  output$total_etp <- renderText({ format(round(selected_effectif()$etp_enseignants[1]), big.mark = " ") })
  output$total_establishments <- renderText({ format(selected_effectif()$etablissements[1], big.mark = " ") })
  output$annual_change <- renderText({
    value <- selected_effectif()$taux_variation_etp[1]
    if (is.na(value)) "Non calculable" else scales::percent(value, accuracy = .1)
  })
  output$effectifs_plot <- renderPlot({
    ggplot(prepared$effectifs, aes(annee, etp_enseignants, color = degre, group = degre)) +
      geom_line(linewidth = 1) + geom_point(size = 3) + theme_minimal(base_size = 13) +
      labs(x = "Année", y = "ETP enseignants", color = "Degré")
  })

  profile_data <- reactive({
    data <- prepared[[input$profile_dimension]]
    subset(data, annee == input$profile_year & degre == input$profile_degree)
  })
  output$profile_plot <- renderPlot({
    ggplot(profile_data(), aes(x = reorder(classe, proportion), y = proportion, fill = classe)) +
      geom_col(show.legend = FALSE) + coord_flip() + scale_y_continuous(labels = scales::label_percent()) +
      theme_minimal(base_size = 13) + labs(x = NULL, y = "Proportion d'ETP")
  })

  mobility_data <- reactive(subset(prepared$mobilite, indicateur == input$mobility_indicator))
  output$mobility_plot <- renderPlot({
    ggplot(mobility_data(), aes(valeur)) + geom_histogram(bins = 25, fill = "#C97A40", color = "white") +
      theme_minimal(base_size = 13) + labs(x = "Valeur du ratio", y = "Départements")
  })
  output$mobility_table <- renderTable({
    x <- mobility_data()[order(-mobility_data()$valeur), c("code_departement", "nom_departement", "valeur")]
    head(x, 15)
  }, striped = TRUE, bordered = TRUE)

  territorial_data <- reactive({
    data <- prepared$territoires[[input$geo_level]]
    name_column <- paste0("nom_", input$geo_level)
    data.frame(territoire = data[[name_column]], valeur = data[[input$territorial_indicator]])
  })
  output$territorial_plot <- renderPlot({
    x <- territorial_data(); x <- head(x[order(-x$valeur), ], 25)
    ggplot(x, aes(reorder(territoire, valeur), valeur)) + geom_col(fill = "#4E8F69") + coord_flip() +
      scale_y_continuous(labels = scales::label_percent()) + theme_minimal(base_size = 12) +
      labs(x = NULL, y = "Proportion d'ETP", subtitle = "25 valeurs les plus élevées")
  })
  output$territorial_map_image <- renderUI({
    tags$img(src = paste0("prepared-figures/territorial/", input$territorial_map, ".png"),
      alt = "Carte territoriale préparée", style = "width:100%;height:auto;")
  })

  output$trajectory_plot <- renderPlot({
    ggplot(prepared$trajectoires, aes(reorder(sequence_presence, etablissements), etablissements)) +
      geom_col(fill = "#35608D") + coord_flip() + theme_minimal(base_size = 12) +
      labs(x = NULL, y = "Établissements")
  })
  output$trajectory_table <- renderTable(prepared$trajectoires, striped = TRUE, bordered = TRUE)

  output$econometric_table <- renderTable({
    x <- prepared$coefficients[, c("term", "odds_ratio", "or_ic95_basse", "or_ic95_haute", "p.value")]
    x[, -1] <- lapply(x[, -1, drop = FALSE], function(value) signif(value, 3)); x
  }, striped = TRUE, bordered = TRUE)
  output$ml_table <- renderTable(prepared$audit_ml, striped = TRUE, bordered = TRUE)
  output$survival_table <- renderTable(prepared$audit_survie, striped = TRUE, bordered = TRUE)
  output$evaluation_table <- renderTable(prepared$evaluation, striped = TRUE, bordered = TRUE)
  output$explainability_table <- renderTable(prepared$explicabilite, striped = TRUE, bordered = TRUE)
  output$quality_table <- renderTable(head(prepared$qualite, 20), striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)

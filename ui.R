source("R/ui_components.R")

ui <- navbarPage(
  title = "DBPG-LAB",

  # SEKCJA: DANE
  tabPanel(
    "Dane",
    tabsetPanel(
      tabPanel(
        "Import",
        h2("Import danych"),
        fileInput("file", "Wybierz plik CSV", accept = ".csv"),
        checkboxInput("header", "Czy plik ma nagłówki?", TRUE),
        selectInput(
          "separator", "Separator",
          choices = c(
            "Przecinek ," = ",",
            "Średnik ;"   = ";",
            "Tabulator"   = "\t"
          ),
          selected = ","
        ),
        actionButton("load_file", "Wczytaj plik"),
        br(), br(),
        textOutput("upload_status")
      ),

      tabPanel(
        "Podgląd",
        h2("Podgląd danych"),
        selectInput(
          "selected_dataset", "Wybierz dane",
          choices = c("iris", "Boston", "mtcars")
        ),
        tableOutput("data_preview"),
        br(),
        h3("Informacje o danych"),
        verbatimTextOutput("data_info")
      )
    )
  ),

    # SEKCJA: DANE
    tabPanel(
        "Regresja",
        tabsetPanel(
        tabPanel(
            "Regresja liniowa",
            h2("Regresja liniowa"),
            regression_controls("linear_dataset", "linear_y", "linear_x",
                            "show_linear_config", "linear_summary")
            ),
        tabPanel(
            "Random Forest",
            h2("Random Forest dla regresji"),
            regression_controls("rf_reg_dataset", "rf_reg_y", "rf_reg_x",
                            "show_rf_reg_config", "rf_reg_summary")
        ),
        tabPanel(
            "SVR",
            h2("Support Vector Regression"),
            regression_controls("svr_dataset", "svr_y", "svr_x",
                            "show_svr_config", "svr_summary")
        )
        )
    ),

  tabPanel("Klasyfikacja", p("// TODO")),

  tabPanel("Klasteryzacja", p("// TODO")),

  tabPanel("Informacje o aplikacji", p("// TODO"))
)
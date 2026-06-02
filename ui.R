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

  tabPanel("Regresja", p("// TODO")),

  tabPanel("Klasyfikacja", p("// TODO")),

  tabPanel("Klasteryzacja", p("// TODO")),

  tabPanel("Informacje o aplikacji", p("// TODO"))
)
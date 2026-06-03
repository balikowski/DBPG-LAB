dataset_select <- function(id) {
  selectInput(
    inputId = id,
    label = "Wybierz zbiór danych:",
    choices = c("iris", "Boston", "mtcars")
  )
}

# Tworzy wspólny interfejs UI dla modeli regresji
regression_controls <- function(dataset_id, y_id, x_id, button_id, summary_id) {
  prefix <- gsub("show_|_config", "", button_id)

  tabsetPanel(
    id = paste0(prefix, "_tabs"),

    # Zakładka: Model 
    tabPanel(
      "Model",
      br(),
      fluidRow(
        column(
          width = 4,
          h4("Konfiguracja modelu"),
          dataset_select(dataset_id),
          selectInput(y_id, "Zmienna zależna Y:", choices = NULL),
          checkboxGroupInput(x_id, "Zmienne niezależne X:", choices = NULL),
          actionButton(button_id, "Załaduj model", class = "btn-primary")
        ),
        column(
          width = 8,
          h4("Szczegóły modelu"),
          verbatimTextOutput(summary_id)
        )
      )
    ),

    # Zakładka: Wykresy
    tabPanel(
      "Wykresy",
      br(),
      plotOutput(paste0(summary_id, "_plot"), height = "500px")
    ),

    # Zakładka: Predykcja
    tabPanel(
      "Predykcja",
      br(),
      fluidRow(
        column(
          width = 5,
          h4("Wprowadź wartości zmiennych"),
          p(
            class = "text-muted",
            "Inputy pojawią się po załadowaniu modelu. ",
            "Uzupełnij wartości zmiennych X i kliknij Oblicz."
          ),
          uiOutput(paste0(prefix, "_pred_inputs")),
          br(),
          actionButton(
            paste0(prefix, "_predict_btn"),
            "Oblicz predykcję",
            class = "btn-success",
            icon  = icon("calculator")
          )
        ),
        column(
          width = 7,
          h4("Wynik predykcji"),
          br(),
          uiOutput(paste0(prefix, "_pred_result"))
        )
      )
    )
  )
}
# Komponenty UI wielokrotnego użytku – używane w ui.R


# Dropdown z wyborem zbioru danych (domyślne opcje: iris, Boston, mtcars)
dataset_select <- function(id) {
  selectInput(
    inputId = id,
    label   = "Wybierz zbiór danych:",
    choices = c("iris", "Boston", "mtcars")
  )
}


# === Kontrolki regresji ===

regression_controls <- function(dataset_id, y_id, x_id, button_id, summary_id) {
  prefix <- gsub("show_|_config", "", button_id)

  tabsetPanel(
    id = paste0(prefix, "_tabs"),

    # zakładka: konfiguracja i wyniki modelu
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

    # zakładka: wykresy diagnostyczne
    tabPanel(
      "Wykresy",
      br(),
      plotOutput(paste0(summary_id, "_plot"), height = "500px")
    ),

    # zakładka: predykcja dla nowej obserwacji
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


# === Kontrolki klasyfikacji ===

classification_controls <- function(dataset_id, target_id, x_id, button_id, summary_id) {
  prefix <- gsub("show_|_config", "", button_id)

  tabsetPanel(
    id = paste0(prefix, "_tabs"),

    # zakładka: konfiguracja i wyniki modelu
    tabPanel(
      "Model",
      br(),
      fluidRow(
        column(
          width = 4,
          h4("Konfiguracja modelu"),
          dataset_select(dataset_id),
          selectInput(target_id, "Kolumna decyzyjna / klasa Y:", choices = NULL),
          checkboxGroupInput(x_id, "Zmienne wejściowe X:", choices = NULL),

          hr(),

          sliderInput(
            inputId = paste0(button_id, "_split"),
            label   = "Podział danych: trening / test",
            min = 50, max = 90, value = 80, step = 5,
            post = "% trening"
          ),

          radioButtons(
            inputId  = paste0(button_id, "_scaling"),
            label    = "Skalowanie danych liczbowych:",
            choices  = c(
              "Wyłączone"     = "none",
              "Standaryzacja" = "standardization",
              "Normalizacja"  = "normalization"
            ),
            selected = "none"
          ),

          radioButtons(
            inputId  = paste0(button_id, "_encoding"),
            label    = "Kodowanie zmiennych kategorycznych:",
            choices  = c(
              "One-hot encoding" = "onehot",
              "Label encoding"   = "label"
            ),
            selected = "onehot",
            inline   = TRUE
          ),

          radioButtons(
            inputId  = paste0(button_id, "_balancing"),
            label    = "Balansowanie klas:",
            choices  = c(
              "Brak"                        = "none",
              "Automatyczne wagi klas"      = "class_weights",
              "Sztuczne generowanie próbek" = "synthetic_samples"
            ),
            selected = "none"
          ),

          actionButton(button_id, "Załaduj model", class = "btn-primary")
        ),
        column(
          width = 8,
          h4("Szczegóły modelu"),
          verbatimTextOutput(summary_id)
        )
      )
    ),

    # zakładka: wykresy diagnostyczne
    tabPanel(
      "Wykresy",
      br(),
      plotOutput(paste0(summary_id, "_plot"), height = "500px")
    ),

    # zakładka: predykcja dla nowej obserwacji
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
            "Uzupełnij wartości zmiennych X i kliknij Klasyfikuj."
          ),
          uiOutput(paste0(prefix, "_pred_inputs")),
          br(),
          actionButton(
            paste0(prefix, "_predict_btn"),
            "Klasyfikuj",
            class = "btn-success",
            icon  = icon("tag")
          )
        ),
        column(
          width = 7,
          h4("Wynik klasyfikacji"),
          br(),
          uiOutput(paste0(prefix, "_pred_result"))
        )
      )
    )
  )
}


# === Kontrolki klasteryzacji ===

clustering_controls <- function(dataset_id, columns_id, button_id, summary_id,
                                extra_ui = NULL) {
  fluidRow(
    column(
      width = 4,
      h4("Konfiguracja klasteryzacji"),
      dataset_select(dataset_id),
      checkboxGroupInput(columns_id, "Wybierz kolumny do klasteryzacji:",
                         choices = NULL),
      extra_ui,
      actionButton(button_id, "Załaduj model", class = "btn-primary")
    ),
    column(
      width = 8,
      h4("Parametry modelu"),
      verbatimTextOutput(summary_id),
      plotOutput(paste0(summary_id, "_plot"), height = "400px")
    )
  )
}

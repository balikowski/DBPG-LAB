source("R/ui_components.R")

ui <- navbarPage(
  title = "DBPG-LAB",

  # ── Dane ──────────────────────────────────────────────────────────────────
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

  # ── Regresja ───────────────────────────────────────────────────────────────
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

  # ── Klasyfikacja ───────────────────────────────────────────────────────────
  tabPanel(
    "Klasyfikacja",
    tabsetPanel(
      tabPanel(
        "Regresja logistyczna",
        h2("Regresja logistyczna"),
        classification_controls("logistic_dataset", "logistic_target", "logistic_x",
                                "show_logistic_config", "logistic_summary")
      ),
      tabPanel(
        "SVM",
        h2("Support Vector Machine"),
        classification_controls("svm_dataset", "svm_target", "svm_x",
                                "show_svm_config", "svm_summary")
      ),
      tabPanel(
        "Random Forest",
        h2("Random Forest dla klasyfikacji"),
        classification_controls("rf_class_dataset", "rf_class_target", "rf_class_x",
                                "show_rf_class_config", "rf_class_summary")
      )
    )
  ),

  # ── Klasteryzacja ──────────────────────────────────────────────────────────
  tabPanel(
    "Klasteryzacja",
    tabsetPanel(
      tabPanel(
        "K-means",
        h2("K-means"),
        clustering_controls(
          "kmeans_dataset", "kmeans_columns",
          "show_kmeans_config", "kmeans_summary",
          extra_ui = sliderInput("kmeans_clusters", "Liczba klastrów:",
                                 min = 2, max = 10, value = 3, step = 1)
        )
      ),
      tabPanel(
        "DBSCAN",
        h2("DBSCAN"),
        clustering_controls(
          "dbscan_dataset", "dbscan_columns",
          "show_dbscan_config", "dbscan_summary",
          extra_ui = sliderInput("dbscan_minpts", "Minimalna liczba sąsiadów (minPts):",
                                 min = 2, max = 20, value = 5, step = 1)
        )
      ),
      tabPanel(
        "Hierarchiczna (hclust)",
        h2("Klasteryzacja hierarchiczna"),
        clustering_controls(
          "meanshift_dataset", "meanshift_columns",
          "show_meanshift_config", "meanshift_summary",
          extra_ui = tagList(
            sliderInput("agnes_k", "Liczba klastrów:",
                        min = 2, max = 10, value = 3, step = 1),
            selectInput("agnes_method", "Metoda łączenia:",
                        choices = c("ward.D2", "complete", "average", "single"),
                        selected = "ward.D2")
          )
        )
      )
    )
  ),

  # ── Informacje ─────────────────────────────────────────────────────────────
  tabPanel(
    "Informacje o aplikacji",
    h1("DBPG-LAB"),
    h3("Opis"),
    p("Aplikacja do importu, podglądu i analizy danych. Umożliwia wczytywanie zbiorów danych w formacie CSV oraz korzystanie z wybranych algorytmów regresji, klasyfikacji i klasteryzacji."),
    h3("Twórcy"),
    tags$ul(
      tags$li(
        tags$strong("Dawid Balikowski"),
        " – ",
        tags$a("github.com/balikowski", href = "https://github.com/balikowski", target = "_blank")
      ),
      tags$li(
        tags$strong("Piotr Graczyk"),
        " – ",
        tags$a("github.com/artenn03x", href = "https://github.com/artenn03x", target = "_blank")
      )
    ),
    p(tags$em("Kierunek: Informatyka | Uczelnia: UKSW")),
    h3("Dokumentacja"),
    p(
      "Dokumentacja projektu dostępna jest w repozytorium: ",
      tags$a("github.com/balikowski/DBPG-LAB", href = "https://github.com/balikowski/DBPG-LAB", target = "_blank")
    )
  )
)
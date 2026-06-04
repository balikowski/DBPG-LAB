source("R/preprocess.R")
source("R/model_spec.R")
source("R/server_helpers.R")

server <- function(input, output, session) {

  # ── Reaktywne dane użytkownika ───────────────────────────────────────────
  uploaded <- reactiveValues(
    data = NULL,
    name = NULL
  )

  get_dataset_by_name <- function(dataset_name) {
    if (is.null(dataset_name)) return(NULL)
    if (dataset_name == "iris")   return(preprocess_data(iris))
    if (dataset_name == "Boston") return(preprocess_data(MASS::Boston))
    if (dataset_name == "mtcars") return(preprocess_data(mtcars))
    if (grepl("^Wgrany:", dataset_name) && !is.null(uploaded$data)) {
      return(uploaded$data)
    }
    return(NULL)
  }

  # ── Aktualizacje list wyboru po zmianie datasetu / zmiennej Y ───────────
  update_regression_inputs(session, input, get_dataset_by_name,
                           "linear_dataset", "linear_y", "linear_x")
  update_regression_inputs(session, input, get_dataset_by_name,
                           "rf_reg_dataset", "rf_reg_y", "rf_reg_x")
  update_regression_inputs(session, input, get_dataset_by_name,
                           "svr_dataset",    "svr_y",    "svr_x")

  update_classification_inputs(session, input, get_dataset_by_name,
                               "logistic_dataset", "logistic_target", "logistic_x")
  update_classification_inputs(session, input, get_dataset_by_name,
                               "svm_dataset",      "svm_target",      "svm_x")
  update_classification_inputs(session, input, get_dataset_by_name,
                               "rf_class_dataset", "rf_class_target", "rf_class_x")

  update_clustering_inputs(session, input, get_dataset_by_name,
                           "kmeans_dataset",    "kmeans_columns")
  update_clustering_inputs(session, input, get_dataset_by_name,
                           "dbscan_dataset",    "dbscan_columns")
  update_clustering_inputs(session, input, get_dataset_by_name,
                           "meanshift_dataset", "meanshift_columns")

  # ── Renderowanie wyników po kliknięciu przycisku ─────────────────────────
  show_regression_config(session, input, output, get_dataset_by_name,
                         "linear_dataset", "linear_y", "linear_x",
                         "show_linear_config", "linear_summary")
  show_regression_config(session, input, output, get_dataset_by_name,
                         "rf_reg_dataset", "rf_reg_y", "rf_reg_x",
                         "show_rf_reg_config", "rf_reg_summary")
  show_regression_config(session, input, output, get_dataset_by_name,
                         "svr_dataset", "svr_y", "svr_x",
                         "show_svr_config", "svr_summary")

  show_classification_config(session, input, output, get_dataset_by_name,
                             "logistic_dataset", "logistic_target", "logistic_x",
                             "show_logistic_config", "logistic_summary")
  show_classification_config(session, input, output, get_dataset_by_name,
                             "svm_dataset", "svm_target", "svm_x",
                             "show_svm_config", "svm_summary")
  show_classification_config(session, input, output, get_dataset_by_name,
                             "rf_class_dataset", "rf_class_target", "rf_class_x",
                             "show_rf_class_config", "rf_class_summary")

  show_clustering_config(session, input, output, get_dataset_by_name,
                         "kmeans_dataset", "kmeans_columns",
                         "show_kmeans_config", "kmeans_summary",
                         "K-means",
                         c("Liczba klastrów" = "kmeans_clusters"))
  show_clustering_config(session, input, output, get_dataset_by_name,
                         "dbscan_dataset", "dbscan_columns",
                         "show_dbscan_config", "dbscan_summary",
                         "DBSCAN",
                         c("Liczba klastrów" = "dbscan_clusters"))
  show_clustering_config(session, input, output, get_dataset_by_name,
                         "meanshift_dataset", "meanshift_columns",
                         "show_meanshift_config", "meanshift_summary",
                         "Mean-shift",
                         c("Liczba klastrów" = "meanshift_clusters"))

  # ── Import pliku CSV ─────────────────────────────────────────────────────
  observeEvent(input$load_file, {
    req(input$file)

    tryCatch({
      raw_data <- read.csv(
        file             = input$file$datapath,
        header           = input$header,
        sep              = input$separator,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )

      validate(
        need(nrow(raw_data) > 0,      "Plik nie zawiera danych."),
        need(ncol(raw_data) >= 2,     "Plik musi mieć co najmniej 2 kolumny."),
        need(nrow(raw_data) <= 100000, "Plik ma zbyt wiele wierszy (limit: 100 000).")
      )

      uploaded$data <- preprocess_data(raw_data)
      uploaded$name <- input$file$name

      new_choices <- c(
        "iris", "Boston", "mtcars",
        paste0("Wgrany: ", uploaded$name)
      )

      all_select_ids <- c(
        "selected_dataset",
        "linear_dataset", "rf_reg_dataset", "svr_dataset",
        "logistic_dataset", "svm_dataset", "rf_class_dataset",
        "kmeans_dataset", "dbscan_dataset", "meanshift_dataset"
      )

      for (id in all_select_ids) {
        updateSelectInput(session, inputId = id, choices = new_choices,
                          selected = paste0("Wgrany: ", uploaded$name))
      }

      showNotification("Plik wczytany i przetworzony pomyślnie.", type = "message")

    }, error = function(e) {
      showNotification(paste("Błąd:", e$message), type = "error")
    })
  })

  # ── Predykcja dla modeli regresji ────────────────────────────────────────
  setup_regression_prediction(session, input, output, get_dataset_by_name,
                              prefix = "linear",
                              dataset_input = "linear_dataset",
                              y_input = "linear_y",
                              x_input = "linear_x",
                              button_input = "show_linear_config",
                              predict_fn = predict_linear)

  setup_regression_prediction(session, input, output, get_dataset_by_name,
                              prefix = "rf_reg",
                              dataset_input = "rf_reg_dataset",
                              y_input = "rf_reg_y",
                              x_input = "rf_reg_x",
                              button_input = "show_rf_reg_config",
                              predict_fn = predict_rf_reg)

  setup_regression_prediction(session, input, output, get_dataset_by_name,
                              prefix = "svr",
                              dataset_input = "svr_dataset",
                              y_input = "svr_y",
                              x_input = "svr_x",
                              button_input = "show_svr_config",
                              predict_fn = predict_svr_reg)

  # ── Predykcja dla modeli klasyfikacji ────────────────────────────────────
  setup_classification_prediction(session, input, output, get_dataset_by_name,
                                  prefix = "logistic",
                                  dataset_input = "logistic_dataset",
                                  target_input  = "logistic_target",
                                  x_input       = "logistic_x",
                                  button_input  = "show_logistic_config",
                                  predict_fn    = predict_logistic)

  setup_classification_prediction(session, input, output, get_dataset_by_name,
                                  prefix = "svm",
                                  dataset_input = "svm_dataset",
                                  target_input  = "svm_target",
                                  x_input       = "svm_x",
                                  button_input  = "show_svm_config",
                                  predict_fn    = predict_svm_class)

  setup_classification_prediction(session, input, output, get_dataset_by_name,
                                  prefix = "rf_class",
                                  dataset_input = "rf_class_dataset",
                                  target_input  = "rf_class_target",
                                  x_input       = "rf_class_x",
                                  button_input  = "show_rf_class_config",
                                  predict_fn    = predict_rf_class)

  # ── Status importu ───────────────────────────────────────────────────────
  output$upload_status <- renderText({
    if (is.null(uploaded$data)) {
      return("Nie wczytano pliku. Dostępne dane: iris, Boston, mtcars.")
    }
    paste("Zaimportowano:", uploaded$name)
  })

  # ── Podgląd danych ───────────────────────────────────────────────────────
  selected_data <- reactive({
    req(input$selected_dataset)
    get_dataset_by_name(input$selected_dataset)
  })

  output$data_preview <- renderTable({
    data <- selected_data()
    req(data)
    head(data, 10)
  })

  output$data_info <- renderPrint({
    data <- selected_data()
    req(data)
    cat("Liczba wierszy:", nrow(data), "\n")
    cat("Liczba kolumn:",  ncol(data), "\n\n")
    cat("Nazwy kolumn:\n")
    print(names(data))
    cat("\nStruktura danych:\n")
    str(data)
  })
}

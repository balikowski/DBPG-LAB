source("R/preprocess.R")
source("R/model_spec.R")
source("R/server_helpers.R")

server <- function(input, output, session) {

    # dane użytkownika
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

  # import pliku CSV
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

  # status importu
  output$upload_status <- renderText({
    if (is.null(uploaded$data)) {
      return("Nie wczytano pliku. Dostępne dane: iris, Boston, mtcars.")
    }
    paste("Zaimportowano:", uploaded$name)
  })

  # podglad danych
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

    # aktualizacja list wyboru po zmianie inputów modeli
    update_regression_inputs(session, input, get_dataset_by_name,
                           "linear_dataset", "linear_y", "linear_x")
    update_regression_inputs(session, input, get_dataset_by_name,
                           "rf_reg_dataset", "rf_reg_y", "rf_reg_x")
    update_regression_inputs(session, input, get_dataset_by_name,
                           "svr_dataset",    "svr_y",    "svr_x")

    # renderowanie wyników 
    show_regression_config(session, input, output, get_dataset_by_name,
                         "linear_dataset", "linear_y", "linear_x",
                         "show_linear_config", "linear_summary")
    show_regression_config(session, input, output, get_dataset_by_name,
                         "rf_reg_dataset", "rf_reg_y", "rf_reg_x",
                         "show_rf_reg_config", "rf_reg_summary")
    show_regression_config(session, input, output, get_dataset_by_name,
                         "svr_dataset", "svr_y", "svr_x",
                         "show_svr_config", "svr_summary")


    # predykcje na regresji
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
    

}
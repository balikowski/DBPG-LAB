# === server_helpers.R ===
#   helpery dla server.R:
#   update_*_inputs  – odświeżają listy wyboru po zmianie datasetu
#   show_*_config    – reagują na przycisk i renderują wyniki modelu


# --- Regresja: aktualizacja selectInput (Y) i checkboxGroupInput (X) ---
update_regression_inputs <- function(session, input, get_dataset_by_name,
                                     dataset_input, y_input, x_input) {
  observeEvent(input[[dataset_input]], {
    data <- get_dataset_by_name(input[[dataset_input]])
    req(data)

    all_cols <- names(data)
    updateSelectInput(session, y_input, choices = all_cols, selected = all_cols[1])
    updateCheckboxGroupInput(session, x_input, choices = all_cols[-1], selected = all_cols[-1])
  }, ignoreInit = FALSE)

  observeEvent(input[[y_input]], {
    data <- get_dataset_by_name(input[[dataset_input]])
    req(data)

    all_cols  <- names(data)
    x_choices <- all_cols[all_cols != input[[y_input]]]
    updateCheckboxGroupInput(session, x_input, choices = x_choices, selected = x_choices)
  }, ignoreInit = TRUE)
}


# --- Klasyfikacja: jak wyżej, ale dla kolumny docelowej (target) ---
update_classification_inputs <- function(session, input, get_dataset_by_name,
                                         dataset_input, target_input, x_input) {
  observeEvent(input[[dataset_input]], {
    data <- get_dataset_by_name(input[[dataset_input]])
    req(data)

    all_cols <- names(data)
    updateSelectInput(session, target_input, choices = all_cols, selected = all_cols[1])
    updateCheckboxGroupInput(session, x_input, choices = all_cols[-1], selected = all_cols[-1])
  }, ignoreInit = FALSE)

  observeEvent(input[[target_input]], {
    data <- get_dataset_by_name(input[[dataset_input]])
    req(data)

    all_cols  <- names(data)
    x_choices <- all_cols[all_cols != input[[target_input]]]
    updateCheckboxGroupInput(session, x_input, choices = x_choices, selected = x_choices)
  }, ignoreInit = TRUE)
}


# --- Klasteryzacja: wypełnia checkboxGroupInput tylko kolumnami numerycznymi ---
update_clustering_inputs <- function(session, input, get_dataset_by_name,
                                     dataset_input, columns_input) {
  observeEvent(input[[dataset_input]], {
    data <- get_dataset_by_name(input[[dataset_input]])
    req(data)

    numeric_cols <- names(data)[sapply(data, is.numeric)]
    updateCheckboxGroupInput(
      session, columns_input,
      choices  = numeric_cols,
      selected = numeric_cols[1:min(2, length(numeric_cols))]
    )
  }, ignoreInit = FALSE)
}


# === Renderowanie wyników po kliknięciu przycisku ===

# --- Regresja ---
show_regression_config <- function(session, input, output, get_dataset_by_name,
                                   dataset_input, y_input, x_input,
                                   button_input, summary_output) {
  observeEvent(input[[button_input]], {
    spec <- build_regression_spec(
      input, get_dataset_by_name,
      dataset_input, y_input, x_input,
      method_id    = button_input,
      method_label = gsub("show_|_config", "", button_input)
    )
    req(spec$data, spec$y_column, length(spec$x_columns) > 0)

    output[[summary_output]] <- renderPrint({
      run_model_for_spec(spec)
    })

    output[[paste0(summary_output, "_plot")]] <- renderPlot({
      plot_model_for_spec(spec)
    }, height = 600)
  })
}


# --- Klasyfikacja ---
show_classification_config <- function(session, input, output, get_dataset_by_name,
                                       dataset_input, target_input, x_input,
                                       button_input, summary_output) {
  observeEvent(input[[button_input]], {
    spec <- build_classification_spec(
      input, get_dataset_by_name,
      dataset_input, target_input, x_input,
      button_input,
      method_id    = button_input,
      method_label = gsub("show_|_config", "", button_input)
    )
    req(spec$data, spec$target_column, length(spec$x_columns) > 0)

    output[[summary_output]] <- renderPrint({
      run_model_for_spec(spec)
    })

    output[[paste0(summary_output, "_plot")]] <- renderPlot({
      plot_model_for_spec(spec)
    }, height = 600)
  })
}


# --- Klasteryzacja ---
show_clustering_config <- function(session, input, output, get_dataset_by_name,
                                   dataset_input, columns_input,
                                   button_input, summary_output,
                                   algorithm_name,
                                   extra_input_ids = NULL) {
  observeEvent(input[[button_input]], {
    spec <- build_clustering_spec(
      input, get_dataset_by_name,
      dataset_input, columns_input,
      method_id       = button_input,
      method_label    = algorithm_name,
      algorithm_name  = algorithm_name,
      extra_input_ids = extra_input_ids
    )
    req(spec$data, spec$columns)

    output[[summary_output]] <- renderPrint({
      run_model_for_spec(spec)
    })

    output[[paste0(summary_output, "_plot")]] <- renderPlot({
      plot_model_for_spec(spec)
    }, height = 600)
  })
}


# === Predykcja – regresja ===
# Generuje dynamiczne inputy po załadowaniu modelu i liczy wynik po kliknięciu.
# prefix        – np. "linear", "rf_reg", "svr"
# predict_fn    – funkcja przyjmująca (spec, new_obs) i zwracająca wartość liczbową
setup_regression_prediction <- function(session, input, output,
                                        get_dataset_by_name,
                                        prefix,
                                        dataset_input, y_input, x_input,
                                        button_input,
                                        predict_fn) {

  pred_inputs_id <- paste0(prefix, "_pred_inputs")
  pred_result_id <- paste0(prefix, "_pred_result")
  pred_btn_id    <- paste0(prefix, "_predict_btn")

  # generuj inputy po załadowaniu modelu
  observeEvent(input[[button_input]], {
    x_cols <- isolate(input[[x_input]])
    data   <- get_dataset_by_name(isolate(input[[dataset_input]]))
    req(x_cols, data)

    output[[pred_inputs_id]] <- renderUI({
      cols <- x_cols
      df   <- data

      input_list <- lapply(cols, function(col) {
        col_data <- df[[col]]
        input_id <- paste0(prefix, "_pred_", gsub("[^a-zA-Z0-9]", "_", col))

        if (is.numeric(col_data)) {
          col_min  <- round(min(col_data, na.rm = TRUE), 3)
          col_max  <- round(max(col_data, na.rm = TRUE), 3)
          col_mean <- round(mean(col_data, na.rm = TRUE), 3)
          numericInput(
            inputId = input_id,
            label   = col,
            value   = col_mean,
            step    = round((col_max - col_min) / 100, 4)
          )
        } else {
          selectInput(
            inputId  = input_id,
            label    = col,
            choices  = unique(na.omit(col_data)),
            selected = col_data[1]
          )
        }
      })

      tagList(input_list)
    })

    output[[pred_result_id]] <- renderUI({ NULL })
  })

  # oblicz predykcję po kliknięciu przycisku
  observeEvent(input[[pred_btn_id]], {
    x_cols <- isolate(input[[x_input]])
    data   <- get_dataset_by_name(isolate(input[[dataset_input]]))
    y_col  <- isolate(input[[y_input]])
    req(x_cols, data, y_col)

    new_obs <- tryCatch({
      vals <- lapply(x_cols, function(col) {
        id  <- paste0(prefix, "_pred_", gsub("[^a-zA-Z0-9]", "_", col))
        val <- input[[id]]
        if (is.null(val)) stop(paste("Brak wartości dla:", col))
        if (is.numeric(data[[col]])) as.numeric(val) else val
      })
      names(vals) <- x_cols
      as.data.frame(vals, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Błąd przy wczytywaniu inputów:", e$message), type = "error")
      return(NULL)
    })

    req(new_obs)

    spec <- build_regression_spec(
      input, get_dataset_by_name,
      dataset_input, y_input, x_input,
      method_id    = button_input,
      method_label = prefix
    )

    pred_val <- tryCatch(
      predict_fn(spec, new_obs),
      error = function(e) {
        showNotification(paste("Błąd predykcji:", e$message), type = "error")
        NULL
      }
    )

    req(!is.null(pred_val))

    output[[pred_result_id]] <- renderUI({
      div(
        class = "well",
        style = "background:#f0fff4; border-left:5px solid #28a745; padding:20px; border-radius:6px;",
        h3(
          style = "color:#155724; margin-top:0;",
          icon("check-circle"), " Wynik predykcji"
        ),
        hr(),
        p(strong("Zmienna docelowa: "), y_col),
        p(strong("Wprowadzone dane:")),
        tags$ul(
          lapply(names(new_obs), function(col) {
            tags$li(paste0(col, ": ", new_obs[[col]]))
          })
        ),
        hr(),
        div(
          style = "font-size:2em; font-weight:bold; color:#155724; text-align:center; padding:15px;",
          paste0(y_col, " = ", round(pred_val, 4))
        )
      )
    })
  })
}


# === Konkretne funkcje predykcji – regresja ===

predict_linear <- function(spec, new_obs) {
  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )
  model <- lm(formula, data = spec$data)
  as.numeric(predict(model, newdata = new_obs))
}

predict_rf_reg <- function(spec, new_obs) {
  res <- implement_rf_regression(spec)
  as.numeric(predict(res$model, newdata = new_obs))
}

predict_svr_reg <- function(spec, new_obs) {
  res <- implement_svr_regression(spec)

  # zastosuj te same parametry skalowania co przy trenowaniu
  num_cols <- res$num_cols
  num_in_x <- intersect(names(new_obs), num_cols)
  if (length(num_in_x) > 0) {
    new_obs[, num_in_x] <- sweep(
      sweep(new_obs[, num_in_x, drop = FALSE], 2,
            res$scale_params$mean[num_in_x], "-"),
      2, res$scale_params$sd[num_in_x], "/")
  }

  pred_scaled <- as.numeric(predict(res$model, newdata = new_obs))
  pred_scaled * res$scale_params$sd[spec$y_column] +
    res$scale_params$mean[spec$y_column]
}


# === Predykcja – klasyfikacja ===
# Generuje dynamiczne inputy i wyświetla przewidywaną klasę po kliknięciu.
# prefix        – np. "logistic", "svm", "rf_class"
# predict_fn    – funkcja przyjmująca (spec, new_obs) i zwracająca nazwę klasy
setup_classification_prediction <- function(session, input, output,
                                            get_dataset_by_name,
                                            prefix,
                                            dataset_input, target_input, x_input,
                                            button_input,
                                            predict_fn) {

  pred_inputs_id <- paste0(prefix, "_pred_inputs")
  pred_result_id <- paste0(prefix, "_pred_result")
  pred_btn_id    <- paste0(prefix, "_predict_btn")

  # generuj inputy po załadowaniu modelu
  observeEvent(input[[button_input]], {
    x_cols <- isolate(input[[x_input]])
    data   <- get_dataset_by_name(isolate(input[[dataset_input]]))
    req(x_cols, data)

    output[[pred_inputs_id]] <- renderUI({
      input_list <- lapply(x_cols, function(col) {
        col_data <- data[[col]]
        input_id <- paste0(prefix, "_pred_", gsub("[^a-zA-Z0-9]", "_", col))

        if (is.numeric(col_data)) {
          col_min  <- round(min(col_data, na.rm = TRUE), 3)
          col_max  <- round(max(col_data, na.rm = TRUE), 3)
          col_mean <- round(mean(col_data, na.rm = TRUE), 3)
          numericInput(input_id, label = col, value = col_mean,
                       step = round((col_max - col_min) / 100, 4))
        } else {
          selectInput(input_id, label = col,
                      choices = unique(na.omit(col_data)), selected = col_data[1])
        }
      })
      tagList(input_list)
    })

    output[[pred_result_id]] <- renderUI({ NULL })
  })

  # oblicz predykcję po kliknięciu przycisku
  observeEvent(input[[pred_btn_id]], {
    x_cols     <- isolate(input[[x_input]])
    data       <- get_dataset_by_name(isolate(input[[dataset_input]]))
    target_col <- isolate(input[[target_input]])
    req(x_cols, data, target_col)

    new_obs <- tryCatch({
      vals <- lapply(x_cols, function(col) {
        id  <- paste0(prefix, "_pred_", gsub("[^a-zA-Z0-9]", "_", col))
        val <- input[[id]]
        if (is.null(val)) stop(paste("Brak wartości dla:", col))
        if (is.numeric(data[[col]])) as.numeric(val) else val
      })
      names(vals) <- x_cols
      as.data.frame(vals, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Błąd przy wczytywaniu inputów:", e$message), type = "error")
      NULL
    })

    req(new_obs)

    spec <- build_classification_spec(
      input, get_dataset_by_name,
      dataset_input, target_input, x_input,
      button_input,
      method_id    = button_input,
      method_label = prefix
    )

    pred_val <- tryCatch(
      predict_fn(spec, new_obs),
      error = function(e) {
        showNotification(paste("Błąd klasyfikacji:", e$message), type = "error")
        NULL
      }
    )

    req(!is.null(pred_val))

    output[[pred_result_id]] <- renderUI({
      div(
        class = "well",
        style = "background:#f0f4ff; border-left:5px solid #3366cc; padding:20px; border-radius:6px;",
        h3(
          style = "color:#1a3399; margin-top:0;",
          icon("tag"), " Wynik klasyfikacji"
        ),
        hr(),
        p(strong("Zmienna docelowa: "), target_col),
        p(strong("Wprowadzone dane:")),
        tags$ul(
          lapply(names(new_obs), function(col) {
            tags$li(paste0(col, ": ", new_obs[[col]]))
          })
        ),
        hr(),
        div(
          style = "font-size:2em; font-weight:bold; color:#1a3399; text-align:center; padding:15px;",
          paste0("Klasa: ", pred_val)
        )
      )
    })
  })
}


# === Konkretne funkcje predykcji – klasyfikacja ===

predict_logistic <- function(spec, new_obs) {
  res <- implement_logistic_classification(spec)

  enc     <- safe_encode_x(res$train_raw, new_obs = new_obs, x_cols = spec$x_columns)
  new_enc <- enc$new_obs
  x_cols  <- enc$x_cols

  pred <- predict(res$model, newdata = new_enc)
  as.character(pred)
}

predict_svm_class <- function(spec, new_obs) {
  res <- implement_svm_classification(spec)

  enc     <- safe_encode_x(res$train_raw, new_obs = new_obs, x_cols = spec$x_columns)
  new_enc <- enc$new_obs
  x_cols  <- enc$x_cols

  # skalowanie identyczne jak przy treningu
  num_x <- intersect(x_cols, names(new_enc)[sapply(new_enc, is.numeric)])
  if (!is.null(res$scale_means) && length(num_x) > 0) {
    new_enc[, num_x] <- scale(new_enc[, num_x, drop = FALSE],
                              center = res$scale_means[num_x],
                              scale  = res$scale_sds[num_x])
  } else if (!is.null(res$scale_mins) && length(num_x) > 0) {
    rng <- res$scale_maxs[num_x] - res$scale_mins[num_x]
    rng[rng == 0] <- 1
    new_enc[, num_x] <- sweep(
      sweep(new_enc[, num_x, drop = FALSE], 2, res$scale_mins[num_x], "-"),
      2, rng, "/")
  }

  as.character(predict(res$model, newdata = new_enc))
}

predict_rf_class <- function(spec, new_obs) {
  res <- implement_rf_classification(spec)

  enc     <- safe_encode_x(res$train_raw, new_obs = new_obs, x_cols = spec$x_columns)
  new_enc <- enc$new_obs
  x_cols  <- enc$x_cols

  as.character(predict(res$model, newdata = new_enc))
}

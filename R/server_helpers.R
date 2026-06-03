# Aktualizacja inputów konfiguracji modelów regresji
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

# Budowanie specyfiku modelu regresji
show_regression_config <- function(session, input, output, get_dataset_by_name,
                                   dataset_input, y_input, x_input,
                                   button_input, summary_output) {
  observeEvent(input[[button_input]], {
    spec <- build_regression_spec(
      input, get_dataset_by_name,
      dataset_input, y_input, x_input,
      method_id = button_input,
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

# Setup predykcji w modelach regresji
setup_regression_prediction <- function(session, input, output,
                                        get_dataset_by_name,
                                        prefix,
                                        dataset_input, y_input, x_input,
                                        button_input,
                                        predict_fn) {

  pred_inputs_id <- paste0(prefix, "_pred_inputs")
  pred_result_id <- paste0(prefix, "_pred_result")
  pred_btn_id    <- paste0(prefix, "_predict_btn")

  # generowanie inputów gdy model jest załadowany
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

    # czyszczenie poprzedniego wyniku
    output[[pred_result_id]] <- renderUI({ NULL })
  })

  # obliczanie predykcji
  observeEvent(input[[pred_btn_id]], {
    x_cols <- isolate(input[[x_input]])
    data   <- get_dataset_by_name(isolate(input[[dataset_input]]))
    y_col  <- isolate(input[[y_input]])
    req(x_cols, data, y_col)

    # Zbierz wartości inputów
    new_obs <- tryCatch({
      vals <- lapply(x_cols, function(col) {
        id  <- paste0(prefix, "_pred_", gsub("[^a-zA-Z0-9]", "_", col))
        val <- input[[id]]
        if (is.null(val)) stop(paste("Brak wartości dla:", col))
        # dopasuj typ do kolumny oryginalnej
        if (is.numeric(data[[col]])) as.numeric(val) else val
      })
      names(vals) <- x_cols
      as.data.frame(vals, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Błąd przy wczytywaniu inputów:", e$message),
                       type = "error")
      return(NULL)
    })

    req(new_obs)

    # budowanie spec
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

    # konkretne funkcje predykcji dla kazdego modelu
    predict_linear <- function(spec, new_obs) {
        formula <- as.formula(
        paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
    )
    model <- lm(formula, data = spec$data)
    as.numeric(predict(model, newdata = new_obs))
    }

    predict_rf_reg <- function(spec, new_obs) {
        model <- implement_rf_regression(spec)

    prediction <- predict(
        model,
        newdata = new_obs
    )
    return(as.numeric(prediction))
    }

    predict_svr_reg <- function(spec, new_obs) {
        
        model_cols <- c(spec$y_column, spec$x_columns)
        data_model <- spec$data[, model_cols, drop = FALSE]

        num_cols <- names(data_model)[sapply(data_model, is.numeric)]

        scale_params <- list(
            mean = sapply(data_model[, num_cols, drop = FALSE], mean, na.rm = TRUE),
            sd   = sapply(data_model[, num_cols, drop = FALSE], sd,   na.rm = TRUE)
        )
        scale_params$sd[scale_params$sd == 0] <- 1

        data_scaled <- data_model
        data_scaled[, num_cols] <- scale(
        data_model[, num_cols, drop = FALSE],
            center = scale_params$mean,
            scale  = scale_params$sd
        )

        x_num_cols <- intersect(spec$x_columns, num_cols)
        gamma_val <- if (length(x_num_cols) > 0) {
            1 / (length(spec$x_columns) * mean(sapply(data_scaled[, x_num_cols, drop = FALSE], var)))
        }  else {
            1 / length(spec$x_columns)
        } 

        formula <- as.formula(paste(
            spec$y_column, "~", paste(spec$x_columns, collapse = " + ")
        ))

        model_svr <- svm(
            formula,
            data    = data_scaled,
            type    = "eps-regression",
            kernel  = "radial",
            cost    = 1,
            epsilon = 0.1,
            gamma   = gamma_val
        )

        new_scaled <- new_obs


        num_new <- intersect(num_cols, names(new_obs))
        if (length(num_new) > 0) {
            new_scaled[, num_new] <- sweep(
            sweep(new_obs[, num_new, drop = FALSE], 2, scale_params$mean[num_new], "-"),
            2, scale_params$sd[num_new], "/"
        )
        }

        missing_cols <- setdiff(spec$x_columns, names(new_scaled))
        if (length(missing_cols) > 0) {
            stop("Brak kolumn w new_obs: ", paste(missing_cols, collapse = ", "))
        }

        pred_scaled <- predict(model_svr, newdata = new_scaled[, spec$x_columns, drop = FALSE])

        y_pred <- as.numeric(pred_scaled) *
            scale_params$sd[spec$y_column] +
            scale_params$mean[spec$y_column]
        return(y_pred)
}
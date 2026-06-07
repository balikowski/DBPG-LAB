# Budowanie specyfikacji modelu na podstawie ustawień GUI.


# --- safe_encode_x ---
# Enkodowanie kolumn X przed dopasowaniem modelu klasyfikacji.
# Kolumny z <= max_cat unikatami -> label encoding; więcej -> 5 cech tekstowych
# (nchar, nwords, ndigits, nupper, nspecial). Nieznane poziomy -> 0.
# Zwraca: $train, $test, $new_obs (opcjonalnie), $x_cols, $encodings, $text_cols
safe_encode_x <- function(train, test = NULL, new_obs = NULL,
                           x_cols, max_cat = 20, encoding_method = "label") {

  encodings <- list()
  text_cols <- c()

  text_features <- function(df, col) {
    v <- as.character(df[[col]])
    result <- data.frame(
      nchar(v),
      lengths(strsplit(trimws(v), "\\s+")),
      nchar(gsub("[^0-9]", "", v)),
      nchar(gsub("[^A-Z]", "", v)),
      nchar(gsub("[[:alnum:][:space:]]", "", v)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(result) <- paste0(col, c("__nchar", "__nwords", "__ndigits", "__nupper", "__nspecial"))
    result
  }

  onehot_expand <- function(df_list, col, lvls) {
    # zwraca listę zmodyfikowanych ramek (train, test, new_obs)
    out <- df_list
    for (nm in names(df_list)) {
      df <- df_list[[nm]]
      if (is.null(df)) { out[[nm]] <- NULL; next }
      v <- as.character(df[[col]])
      dummies <- as.data.frame(
        lapply(lvls, function(lv) as.integer(v == lv)),
        stringsAsFactors = FALSE
      )
      names(dummies) <- paste0(col, "__", lvls)
      out[[nm]] <- cbind(df[, setdiff(names(df), col), drop = FALSE], dummies)
    }
    out
  }

  for (col in x_cols) {
    col_data <- train[[col]]
    if (is.numeric(col_data)) next

    uniq <- unique(na.omit(as.character(col_data)))

    if (length(uniq) <= max_cat) {
      lvls <- sort(uniq)
      encodings[[col]] <- lvls

      if (encoding_method == "onehot") {
        res   <- onehot_expand(list(train = train, test = test, new_obs = new_obs), col, lvls)
        train <- res$train; test <- res$test; new_obs <- res$new_obs
      } else {
        # label encoding
        encode_col <- function(v, lvls) {
          v <- as.character(v)
          ifelse(v %in% lvls, match(v, lvls), 0L)
        }
        train[[col]] <- encode_col(train[[col]], lvls)
        if (!is.null(test))    test[[col]]    <- encode_col(test[[col]],    lvls)
        if (!is.null(new_obs)) new_obs[[col]] <- encode_col(new_obs[[col]], lvls)
      }

    } else {
      text_cols <- c(text_cols, col)
      train <- cbind(train[, setdiff(names(train), col), drop = FALSE], text_features(train, col))
      if (!is.null(test))
        test  <- cbind(test[,  setdiff(names(test),  col), drop = FALSE], text_features(test,  col))
      if (!is.null(new_obs))
        new_obs <- cbind(new_obs[, setdiff(names(new_obs), col), drop = FALSE], text_features(new_obs, col))
    }
  }

  # zaktualizuj x_cols
  new_x_cols <- x_cols
  for (col in text_cols) {
    new_x_cols <- setdiff(new_x_cols, col)
    new_x_cols <- c(new_x_cols,
                    paste0(col, c("__nchar","__nwords","__ndigits","__nupper","__nspecial")))
  }
  if (encoding_method == "onehot") {
    for (col in names(encodings)) {
      new_x_cols <- setdiff(new_x_cols, col)
      new_x_cols <- c(new_x_cols, paste0(col, "__", encodings[[col]]))
    }
  }

  list(
    train     = train,
    test      = test,
    new_obs   = new_obs,
    x_cols    = new_x_cols,
    encodings = encodings,
    text_cols = text_cols
  )
}


# --- Funkcje budujące specyfikację ---
# Zbierają parametry z GUI i pakują je w listę dla funkcji implement_*

build_regression_spec <- function(input, get_dataset_by_name,
                                  dataset_input, y_input, x_input,
                                  method_id = NULL,
                                  method_label = NULL,
                                  algorithm = "linear") {
  dataset_name  <- isolate(input[[dataset_input]])
  y_column      <- isolate(input[[y_input]])
  x_columns     <- isolate(input[[x_input]])
  data          <- get_dataset_by_name(dataset_name)

  list(
    problem_type = "regression",
    method_id    = method_id,
    method_label = method_label,
    algorithm    = algorithm,
    dataset_name = dataset_name,
    data         = data,
    y_column     = y_column,
    x_columns    = x_columns
  )
}

build_classification_spec <- function(input, get_dataset_by_name,
                                      dataset_input, target_input, x_input,
                                      button_input,
                                      method_id = NULL,
                                      method_label = NULL,
                                      algorithm = "classification") {
  dataset_name     <- isolate(input[[dataset_input]])
  target_column    <- isolate(input[[target_input]])
  x_columns        <- isolate(input[[x_input]])
  train_percent    <- isolate(input[[paste0(button_input, "_split")]])
  scaling_method   <- isolate(input[[paste0(button_input, "_scaling")]])
  encoding_method  <- isolate(input[[paste0(button_input, "_encoding")]])
  balancing_mode   <- isolate(input[[paste0(button_input, "_balancing")]])
  data             <- get_dataset_by_name(dataset_name)

  list(
    problem_type    = "classification",
    method_id       = method_id,
    method_label    = method_label,
    algorithm       = algorithm,
    dataset_name    = dataset_name,
    data            = data,
    target_column   = target_column,
    x_columns       = x_columns,
    train_percent   = train_percent,
    test_percent    = 100 - train_percent,
    scaling_method  = scaling_method,
    encoding_method = encoding_method,
    balancing_mode  = balancing_mode
  )
}

build_clustering_spec <- function(input, get_dataset_by_name,
                                  dataset_input, columns_input,
                                  method_id,
                                  method_label,
                                  algorithm_name,
                                  extra_input_ids = NULL) {
  dataset_name     <- isolate(input[[dataset_input]])
  selected_columns <- isolate(input[[columns_input]])
  data             <- get_dataset_by_name(dataset_name)

  extra_params <- NULL
  if (!is.null(extra_input_ids)) {
    extra_params <- lapply(extra_input_ids, function(id) isolate(input[[id]]))
    names(extra_params) <- names(extra_input_ids)
  }

  list(
    problem_type  = "clustering",
    method_id     = method_id,
    method_label  = method_label,
    algorithm     = algorithm_name,
    dataset_name  = dataset_name,
    data          = data,
    columns       = selected_columns,
    extra_params  = extra_params
  )
}

run_model_for_spec <- function(spec) {
  if (is.null(spec$method_id)) {
    cat("Brak method_id w spec. Nie można wybrać metody modelu.\n")
    return(invisible(NULL))
  }

  switch(spec$method_id,
    show_linear_config = implement_linear_regression(spec),
    show_rf_reg_config = implement_rf_regression(spec),
    show_svr_config    = implement_svr_regression(spec),
    show_logistic_config = implement_logistic_classification(spec),
    show_svm_config      = implement_svm_classification(spec),
    show_rf_class_config = implement_rf_classification(spec),
    show_kmeans_config   = implement_kmeans_clustering(spec),
    show_dbscan_config   = implement_dbscan_clustering(spec),
    show_meanshift_config = implement_hclust_clustering(spec),
    cat("Brak implementacji dla:", spec$method_id, "\n")
  )
}

plot_model_for_spec <- function(spec) {
  if (is.null(spec$method_id)) {
    plot.new()
    text(0.5, 0.5, "Brak method_id w spec.")
    return(invisible(NULL))
  }

  switch(spec$method_id,
    show_linear_config = plot_linear_regression(spec),
    show_rf_reg_config = plot_rf_regression(spec),
    show_svr_config    = plot_svr_regression(spec),
    show_logistic_config = plot_logistic_classification(spec),
    show_svm_config      = plot_svm_classification(spec),
    show_rf_class_config = plot_rf_classification(spec),
    show_kmeans_config   = plot_kmeans_clustering(spec),
    show_dbscan_config   = plot_dbscan_clustering(spec),
    show_meanshift_config = plot_hclust_clustering(spec),
    {
      plot.new()
      text(0.5, 0.5, paste("Brak wykresu dla:", spec$method_id))
    }
  )
}

# === Implementacje modeli ===

# --- Regresja liniowa ---
implement_linear_regression <- function(spec) {
  train_pct <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8

  set.seed(123)
  n     <- nrow(spec$data)
  idx   <- sample(seq_len(n), size = floor(train_pct * n))
  train <- spec$data[idx,  , drop = FALSE]
  test  <- spec$data[-idx, , drop = FALSE]

  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )

  model <- lm(formula, data = train)

  y_true <- test[[spec$y_column]]
  y_pred <- predict(model, newdata = test)

  n_test <- length(y_true)
  p      <- length(spec$x_columns)
  mse    <- mean((y_true - y_pred)^2)
  rmse   <- sqrt(mse)
  r2     <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  r2adj  <- 1 - ((1 - r2) * (n_test - 1) / (n_test - p - 1))

  cat("====== OCENA MODELU ======\n")
  cat("Podział:", floor(train_pct * 100), "% trening /",
      round((1 - train_pct) * 100), "% test\n\n")
  cat("R²            =", round(r2, 3), "\n")
  cat("Adjusted R²   =", round(r2adj, 3), "\n")
  cat("MSE           =", round(mse, 3), "\n")
  cat("RMSE          =", round(rmse, 3), "\n\n")

  cat("====== RÓWNANIE MODELU (trening) ======\n")
  coefs <- coef(model)
  formula_text <- paste0("Y = ", round(coefs[1], 2))
  for (i in 2:length(coefs)) {
    value    <- round(coefs[i], 3)
    variable <- names(coefs[i])
    sign     <- ifelse(value >= 0, " + ", " - ")
    formula_text <- paste0(formula_text, "\n", sign, abs(value), " * ", variable)
  }
  cat(formula_text)

  return(invisible(list(model = model, train = train, test = test,
                        y_true = y_true, y_pred = y_pred)))
}


plot_linear_regression <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(ggcorrplot)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  res   <- implement_linear_regression(spec)
  model <- res$model

  # wykresy na zbiorze testowym
  preds <- data.frame(
    Observed  = res$y_true,
    Predicted = res$y_pred
  )

  residual_data <- data.frame(
    predicted = res$y_pred,
    residuals = res$y_true - res$y_pred
  )

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # obserwowane vs przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # macierz korelacji
  all_vars <- c(spec$y_column, spec$x_columns)
  dane_kor <- spec$data[, all_vars, drop = FALSE]

  is_numeric_col     <- sapply(dane_kor, is.numeric)
  dane_kor_numeric   <- dane_kor[, is_numeric_col, drop = FALSE]

  if (ncol(dane_kor_numeric) >= 2) {
    macierz_korelacji <- cor(dane_kor_numeric, use = "complete.obs")

    p4 <- ggcorrplot(
      macierz_korelacji,
      hc.order = TRUE,
      type     = "lower",
      lab      = TRUE,
      lab_size = 3,
      colors   = c("#6D9EC1", "white", "#E46726"),
      title    = "Macierz korelacji"
    ) +
      base_theme +
      theme(
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position  = "right"
      )
  } else {
    p4 <- ggplot() +
      annotate(
        "text", x = 1, y = 1,
        label = "Brak wystarczającej liczby\nzmiennych numerycznych do korelacji",
        size  = 5
      ) +
      theme_void()
  }

  # współczynniki modelu z 95% przedziałami ufności
  coef_df <- as.data.frame(confint(model))
  coef_df$estimate  <- coef(model)
  coef_df$term      <- rownames(coef_df)
  # Pomijamy wyraz wolny – skupiamy się na zmiennych predykcyjnych
  coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]
  colnames(coef_df)[1:2] <- c("lower", "upper")

  p5 <- ggplot(coef_df, aes(x = reorder(term, estimate),
                             y = estimate,
                             ymin = lower,
                             ymax = upper)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(width = 0.25, color = "steelblue", linewidth = 0.9) +
    geom_point(size = 3, color = "steelblue") +
    coord_flip() +
    labs(
      title = "Współczynniki modelu (95% CI)",
      x     = "Zmienna",
      y     = "Wartosc współczynnika"
    ) +
    base_theme

  # histogram reszt z krzywą normalną
  res_vals <- residuals(model)
  res_sd   <- sd(res_vals)
  res_mean <- mean(res_vals)

  p6 <- ggplot(data.frame(res = res_vals), aes(x = res)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins  = 30,
      fill  = "steelblue",
      alpha = 0.6,
      color = "white"
    ) +
    stat_function(
      fun  = dnorm,
      args = list(mean = res_mean, sd = res_sd),
      color = "red",
      linewidth = 1
    ) +
    labs(
      title = "Rozkład reszt",
      x     = "Reszty",
      y     = "Gestosc"
    ) +
    base_theme

  # Układ 2x3
  (p1 | p2 | p3) /
  (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka modelu regresji liniowej",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    )
}




implement_rf_regression <- function(spec) {
  library(randomForest)
  set.seed(123)

  train_pct <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8

  n     <- nrow(spec$data)
  idx   <- sample(seq_len(n), size = floor(train_pct * n))
  train <- spec$data[idx,  , drop = FALSE]
  test  <- spec$data[-idx, , drop = FALSE]

  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )

  model_rf <- randomForest(formula, data = train)

  y_true <- test[[spec$y_column]]
  y_pred <- predict(model_rf, newdata = test)

  n_test <- length(y_true)
  p      <- length(spec$x_columns)
  r2     <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  adj_r2 <- 1 - ((1 - r2) * (n_test - 1) / (n_test - p - 1))
  mse    <- mean((y_true - y_pred)^2)
  rmse   <- sqrt(mse)

  cat("====== OCENA MODELU ======\n")
  cat("Podział:", floor(train_pct * 100), "% trening /",
      round((1 - train_pct) * 100), "% test\n\n")
  cat("R²            =", round(r2, 3), "\n")
  cat("Adjusted R²   =", round(adj_r2, 3), "\n")
  cat("MSE           =", round(mse, 3), "\n")
  cat("RMSE          =", round(rmse, 3), "\n")

  cat("\n====== WAŻNOŚĆ ZMIENNYCH ======\n")
  imp <- importance(model_rf)
  for (i in rownames(imp)) {
    cat(i, ":", round(imp[i, 1], 3), "\n")
  }

  return(invisible(list(model = model_rf, train = train, test = test,
                        y_true = y_true, y_pred = y_pred)))
}

plot_rf_regression <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(ggcorrplot)
  library(randomForest)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  res       <- implement_rf_regression(spec)
  model     <- res$model
  observed  <- res$y_true
  predicted <- res$y_pred

  # Dane: observed vs predicted
  preds <- data.frame(
    Observed  = observed,
    Predicted = predicted
  )

  # Dane: reszty
  residual_data <- data.frame(
    predicted = predicted,
    residuals = observed - predicted
  )

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # obserwowane vs przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # macierz korelacji
  all_vars <- c(spec$y_column, spec$x_columns)
  dane_kor <- spec$data[, all_vars, drop = FALSE]

  is_numeric_col   <- sapply(dane_kor, is.numeric)
  dane_kor_numeric <- dane_kor[, is_numeric_col, drop = FALSE]

  if (ncol(dane_kor_numeric) >= 2) {
    macierz_korelacji <- cor(dane_kor_numeric, use = "complete.obs")

    p4 <- ggcorrplot(
      macierz_korelacji,
      hc.order = TRUE,
      type     = "lower",
      lab      = TRUE,
      lab_size = 3,
      colors   = c("#6D9EC1", "white", "#E46726"),
      title    = "Macierz korelacji"
    ) +
      base_theme +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  } else {
    p4 <- ggplot() +
      annotate(
        "text", x = 1, y = 1,
        label = "Brak wystarczającej liczby\nzmiennych numerycznych do korelacji",
        size  = 5
      ) +
      theme_void()
  }

  # ważność zmiennych
  imp      <- as.data.frame(importance(model))
  imp$term <- rownames(imp)

  # randomForest zwraca %IncMSE gdy importance=TRUE; domyślnie IncNodePurity
  imp_col <- if ("%IncMSE" %in% colnames(imp)) "%IncMSE" else colnames(imp)[1]

  p5 <- ggplot(imp, aes(x = reorder(term, .data[[imp_col]]),
                         y = .data[[imp_col]])) +
    geom_col(fill = "coral", alpha = 0.85) +
    coord_flip() +
    labs(
      title = paste0("Ważność zmiennych (", imp_col, ")"),
      x     = "Zmienna",
      y     = imp_col
    ) +
    base_theme

  # błąd OOB w zależności od liczby drzew
  oob_df <- data.frame(
    ntree    = seq_along(model$mse),
    OOB_MSE  = model$mse
  )

  p6 <- ggplot(oob_df, aes(x = ntree, y = OOB_MSE)) +
    geom_line(color = "darkgreen", linewidth = 0.8) +
    labs(
      title = "Błąd OOB vs liczba drzew",
      x     = "Liczba drzew",
      y     = "OOB MSE"
    ) +
    base_theme

  # Układ 2x3
  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka modelu Random Forest",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    )
}

implement_svr_regression <- function(spec) {
  library(e1071)
  set.seed(123)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) return(NULL)

  train_pct  <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8
  data       <- spec$data
  model_cols <- c(spec$y_column, spec$x_columns)

  n     <- nrow(data)
  idx   <- sample(seq_len(n), size = floor(train_pct * n))
  train <- data[idx,  model_cols, drop = FALSE]
  test  <- data[-idx, model_cols, drop = FALSE]

  # kolumny numeryczne i parametry skalowania
  num_cols <- names(train)[sapply(train, is.numeric)]

  scale_params <- list(
    mean = sapply(train[, num_cols, drop = FALSE], mean, na.rm = TRUE),
    sd   = sapply(train[, num_cols, drop = FALSE], sd,   na.rm = TRUE)
  )
  scale_params$sd[scale_params$sd == 0] <- 1

  train_scaled      <- train
  test_scaled       <- test
  train_scaled[, num_cols] <- scale(train[, num_cols, drop = FALSE],
                                    center = scale_params$mean, scale = scale_params$sd)
  test_scaled[, num_cols]  <- scale(test[,  num_cols, drop = FALSE],
                                    center = scale_params$mean, scale = scale_params$sd)

  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )

  x_num_cols <- intersect(spec$x_columns, num_cols)
  gamma_val  <- if (length(x_num_cols) > 0) {
    1 / (length(spec$x_columns) * mean(sapply(train_scaled[, x_num_cols, drop = FALSE], var)))
  } else {
    1 / length(spec$x_columns)
  }

  model_svr <- svm(formula, data = train_scaled,
                   type = "eps-regression", kernel = "radial",
                   cost = 1, epsilon = 0.1, gamma = gamma_val,
                   scale = FALSE)   # dane już skalowane ręcznie

  pred_scaled <- predict(model_svr, newdata = test_scaled)
  y_pred <- pred_scaled * scale_params$sd[spec$y_column] +
            scale_params$mean[spec$y_column]
  y_true <- test[[spec$y_column]]

  n_test <- length(y_true)
  p      <- length(spec$x_columns)
  mse    <- mean((y_true - y_pred)^2)
  rmse   <- sqrt(mse)
  mae    <- mean(abs(y_true - y_pred))
  r2     <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  adj_r2 <- 1 - ((1 - r2) * (n_test - 1) / (n_test - p - 1))

  cat("====== OCENA MODELU ======\n")
  cat("Podział:", floor(train_pct * 100), "% trening /",
      round((1 - train_pct) * 100), "% test\n\n")
  cat("R²            =", round(r2, 3), "\n")
  cat("Adjusted R²   =", round(adj_r2, 3), "\n")
  cat("MSE           =", round(mse, 3), "\n")
  cat("RMSE          =", round(rmse, 3), "\n")
  cat("MAE           =", round(mae, 3), "\n")

  cat("\n====== PARAMETRY MODELU ======\n")
  cat("Kernel:", model_svr$kernel, "\n")
  cat("Cost:", model_svr$cost, "\n")
  cat("Gamma:", round(model_svr$gamma, 4), "\n")
  cat("Epsilon:", model_svr$epsilon, "\n")
  cat("\n====== SUPPORT VECTORS ======\n")
  cat("Liczba support vectors:", model_svr$tot.nSV, "\n")

  return(invisible(list(model = model_svr,
                        train_scaled = train_scaled, test_scaled = test_scaled,
                        y_true = y_true, y_pred = y_pred,
                        scale_params = scale_params, num_cols = num_cols,
                        eps = model_svr$epsilon)))
}

plot_svr_regression <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(ggcorrplot)
  library(e1071)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  res   <- implement_svr_regression(spec)
  model <- res$model

  observed        <- res$y_true
  predicted       <- res$y_pred
  residual_values <- observed - predicted
  eps             <- model$epsilon * res$scale_params$sd[spec$y_column]

  preds <- data.frame(Observed = observed, Predicted = predicted)
  residual_data <- data.frame(predicted = predicted, residuals = residual_values)

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # obserwowane vs przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # macierz korelacji – używamy danych treningowych (przed skalowaniem)
  dane_kor         <- spec$data[, c(spec$y_column, spec$x_columns), drop = FALSE]
  numeric_cols     <- sapply(dane_kor, is.numeric)
  dane_kor_numeric <- dane_kor[, numeric_cols, drop = FALSE]

  if (ncol(dane_kor_numeric) >= 2) {
    corr_matrix <- cor(dane_kor_numeric, use = "complete.obs")

    p4 <- ggcorrplot(
      corr_matrix,
      hc.order = TRUE,
      type     = "lower",
      lab      = TRUE,
      lab_size = 3,
      colors   = c("#6D9EC1", "white", "#E46726"),
      title    = "Macierz korelacji"
    ) +
      base_theme +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  } else {
    p4 <- ggplot() +
      annotate(
        "text", x = 1, y = 1,
        label = "Brak wystarczającej liczby\nzmiennych numerycznych do korelacji",
        size  = 5
      ) +
      theme_void()
  }

  # epsilon-tube z zaznaczonymi support vectors
  idx <- seq_along(observed)
  idx <- seq_along(observed)

  tube_df <- data.frame(
    idx       = idx,
    observed  = observed,
    predicted = predicted,
    is_sv     = abs(residual_values) >= eps  # punkty poza epsilon-tube ~ support vectors
  )

  p5 <- ggplot(tube_df, aes(x = idx)) +
    geom_ribbon(
      aes(
        ymin = predicted - eps,
        ymax = predicted + eps
      ),
      fill  = "steelblue",
      alpha = 0.2
    ) +
    geom_line(aes(y = predicted), color = "steelblue", linewidth = 0.8) +
    geom_point(
      data = tube_df[!tube_df$is_sv, ],
      aes(y = observed),
      alpha = 0.5,
      size  = 1.5,
      color = "grey40"
    ) +
    geom_point(
      data = tube_df[tube_df$is_sv, ],
      aes(y = observed),
      color = "red",
      size  = 2.5,
      shape = 17
    ) +
    labs(
      title    = "Epsilon-tube SVR",
      subtitle = "Czerwone trójkąty = support vectors",
      x        = "Indeks obserwacji",
      y        = spec$y_column
    ) +
    base_theme

  # rozkład |reszt| względem epsilon-tube
  abs_res_df <- data.frame(
    abs_residual = abs(residual_values),
    status       = ifelse(
      abs(residual_values) <= eps,
      "W epsilon-tube",
      "Poza epsilon-tube"
    )
  )

  pct_in <- round(
    100 * mean(abs(residual_values) <= eps), 1
  )

  p6 <- ggplot(abs_res_df, aes(x = abs_residual, fill = status)) +
    geom_histogram(bins = 30, alpha = 0.75, color = "white") +
    geom_vline(
      xintercept = eps,
      color      = "red",
      linetype   = "dashed",
      linewidth  = 0.9
    ) +
    scale_fill_manual(
      values = c("W epsilon-tube" = "steelblue",
                 "Poza epsilon-tube" = "coral")
    ) +
    labs(
      title    = "Rozkład |reszt| vs ε",
      subtitle = paste0(pct_in, "% obserwacji mieści się w ε-tube"),
      x        = "|Reszta|",
      y        = "Liczba obserwacji",
      fill     = NULL
    ) +
    base_theme +
    theme(legend.position = "bottom")

  # Układ 2x3
  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka modelu SVR",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    )
}

implement_logistic_classification <- function(spec) {
  library(nnet)

  data          <- spec$data
  target        <- spec$target_column
  x_cols        <- spec$x_columns
  train_pct     <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8
  scaling       <- if (!is.null(spec$scaling_method))  spec$scaling_method  else "none"
  encoding      <- if (!is.null(spec$encoding_method)) spec$encoding_method else "onehot"
  balancing     <- if (!is.null(spec$balancing_mode))  spec$balancing_mode  else "none"

  set.seed(123)

  # podział trening / test
  n         <- nrow(data)
  idx       <- sample(seq_len(n), size = floor(train_pct * n))
  train_raw <- data[idx,  , drop = FALSE]   # przed encodingiem – dla predykcji
  train     <- train_raw
  test      <- data[-idx, , drop = FALSE]

  # enkodowanie kolumn kategorycznych
  enc    <- safe_encode_x(train, test, x_cols = x_cols, encoding_method = encoding)
  train  <- enc$train
  test   <- enc$test
  x_cols <- enc$x_cols

  if (length(enc$text_cols) > 0)
    cat("Kolumny tekstowe zamienione na cechy numeryczne:",
        paste(enc$text_cols, collapse = ", "), "\n")

  # skalowanie zmiennych numerycznych
  num_x <- intersect(x_cols, names(train)[sapply(train, is.numeric)])

  scale_means <- NULL; scale_sds <- NULL; scale_mins <- NULL; scale_maxs <- NULL

  if (scaling == "standardization" && length(num_x) > 0) {
    means <- sapply(train[, num_x, drop = FALSE], mean, na.rm = TRUE)
    sds   <- sapply(train[, num_x, drop = FALSE], sd,   na.rm = TRUE)
    sds[sds == 0] <- 1
    scale_means <- means; scale_sds <- sds
    train[, num_x] <- scale(train[, num_x, drop = FALSE], center = means, scale = sds)
    test[,  num_x] <- scale(test[,  num_x, drop = FALSE], center = means, scale = sds)
  } else if (scaling == "normalization" && length(num_x) > 0) {
    mins  <- sapply(train[, num_x, drop = FALSE], min, na.rm = TRUE)
    maxs  <- sapply(train[, num_x, drop = FALSE], max, na.rm = TRUE)
    rng   <- maxs - mins; rng[rng == 0] <- 1
    scale_mins <- mins; scale_maxs <- maxs
    train[, num_x] <- sweep(sweep(train[, num_x, drop = FALSE], 2, mins, "-"), 2, rng, "/")
    test[,  num_x] <- sweep(sweep(test[,  num_x, drop = FALSE], 2, mins, "-"), 2, rng, "/")
  }

  # wagi klas
  class_weights <- NULL
  if (balancing == "class_weights") {
    tbl   <- table(train[[target]])
    class_weights <- (nrow(train) / (length(tbl) * tbl))
  }

  # formuła i dopasowanie modelu
  y_fact  <- as.factor(train[[target]])
  classes <- levels(y_fact)
  k       <- length(classes)

  formula <- as.formula(paste(
    paste0("factor(`", target, "`)"),
    "~",
    paste(paste0("`", x_cols, "`"), collapse = " + ")
  ))

  if (k == 2) {
    wts <- if (!is.null(class_weights)) class_weights[as.character(train[[target]])] else NULL
    model <- glm(formula, data = train, family = binomial(), weights = wts)
  } else {
    model <- multinom(formula, data = train, trace = FALSE)
  }

  # predykcja na zbiorze testowym
  pred_test <- if (k == 2) {
    probs <- predict(model, newdata = test, type = "response")
    factor(ifelse(probs >= 0.5, classes[2], classes[1]), levels = classes)
  } else {
    predict(model, newdata = test)
  }

  true_test <- factor(test[[target]], levels = classes)
  cm        <- table(Prawdziwa = true_test, Przewidywana = pred_test)

  acc <- sum(diag(cm)) / sum(cm)

  # precision / recall / F1 per klasa
  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0

  cat("====== OCENA MODELU ======\n")
  cat("Podział: ", floor(train_pct * 100), "% trening /", round((1 - train_pct) * 100), "% test\n")
  cat("Skalowanie:", scaling, "\n")
  cat("Balansowanie:", balancing, "\n\n")
  cat("Dokładność (Accuracy):", round(acc, 4), "\n\n")
  cat("Macierz pomyłek:\n")
  print(cm)
  cat("\n")
  cat("Metryki per klasa:\n")
  metrics_df <- data.frame(
    Precision = round(prec, 4),
    Recall    = round(rec,  4),
    F1        = round(f1,   4)
  )
  print(metrics_df)

  if (k == 2) {
    cat("\n====== WSPÓŁCZYNNIKI MODELU ======\n")
    coef_df <- summary(model)$coefficients
    print(round(coef_df, 4))
  }

  return(invisible(list(model = model, cm = cm, acc = acc, classes = classes,
                        test = test, pred_test = pred_test, true_test = true_test,
                        k = k, target = target, x_cols = x_cols,
                        train_raw = train_raw,
                        scale_means = scale_means, scale_sds = scale_sds,
                        scale_mins = scale_mins, scale_maxs = scale_maxs)))
}

plot_logistic_classification <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(nnet)

  if (is.null(spec$target_column)) return()

  res <- implement_logistic_classification(spec)
  if (is.null(res)) return()

  model      <- res$model
  cm         <- res$cm
  classes    <- res$classes
  true_test  <- res$true_test
  pred_test  <- res$pred_test
  k          <- res$k

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # macierz pomyłek (heatmap)
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Prawdziwa", "Przewidywana", "Liczba")

  p1 <- ggplot(cm_df, aes(x = Przewidywana, y = Prawdziwa, fill = Liczba)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Liczba), size = 6, fontface = "bold") +
    scale_fill_gradient(low = "#eaf4ff", high = "#2171b5") +
    labs(title = "Macierz pomyłek", x = "Przewidywana klasa", y = "Prawdziwa klasa") +
    base_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))

  # precision / Recall / F1 per klasa
  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0

  metrics_long <- data.frame(
    Klasa   = rep(classes, 3),
    Metryka = rep(c("Precision", "Recall", "F1"), each = length(classes)),
    Wartosc = c(prec, rec, f1)
  )

  p2 <- ggplot(metrics_long, aes(x = Klasa, y = Wartosc, fill = Metryka)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Precision / Recall / F1 per klasa", x = "Klasa", y = "Wartosc", fill = NULL) +
    base_theme +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30, hjust = 1))

  # rozkład prawdopodobieństw dla klasy pozytywnej (binarna) lub pewność (multiclass)
  test_data <- res$test
  if (k == 2) {
    probs_pos <- predict(model, newdata = test_data, type = "response")
    prob_df <- data.frame(
      Prob   = probs_pos,
      Klasa  = res$true_test
    )
    p3 <- ggplot(prob_df, aes(x = Prob, fill = Klasa)) +
      geom_histogram(bins = 25, alpha = 0.7, position = "identity", color = "white") +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Rozkład p(klasa pozytywna)", x = "Prawdopodobieństwo", y = "Liczba", fill = "Klasa") +
      base_theme +
      theme(legend.position = "bottom")
  } else {
    # dla multiclass: histogram dokładności
    correct_df <- data.frame(Wynik = ifelse(res$true_test == res$pred_test, "Poprawna", "Bledna"))
    p3 <- ggplot(correct_df, aes(x = Wynik, fill = Wynik)) +
      geom_bar(alpha = 0.85) +
      scale_fill_manual(values = c("Poprawna" = "#2ca02c", "Bledna" = "#d62728")) +
      labs(title = "Poprawne vs błędne predykcje", x = NULL, y = "Liczba obserwacji") +
      base_theme +
      theme(legend.position = "none")
  }

  # rozkład klas w zbiorze testowym
  class_dist <- as.data.frame(table(Klasa = res$true_test))
  p4 <- ggplot(class_dist, aes(x = Klasa, y = Freq, fill = Klasa)) +
    geom_col(alpha = 0.85) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Rozkład klas (zbiór testowy)", x = "Klasa", y = "Liczba obserwacji") +
    base_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))

  # rOC (tylko binarna) lub macierz korelacji cech
  num_x <- intersect(spec$x_columns, names(spec$data)[sapply(spec$data, is.numeric)])

  if (k == 2 && length(num_x) >= 1) {
    probs_all <- predict(model, newdata = test_data, type = "response")
    true_bin  <- as.integer(res$true_test == classes[2])
    roc_df    <- data.frame(prob = probs_all, label = true_bin)
    roc_df    <- roc_df[order(-roc_df$prob), ]
    n_pos     <- sum(true_bin)
    n_neg     <- length(true_bin) - n_pos
    roc_curve <- data.frame(
      FPR = c(0, cumsum(1 - roc_df$label) / max(n_neg, 1), 1),
      TPR = c(0, cumsum(roc_df$label)      / max(n_pos, 1), 1)
    )
    auc_val <- sum(diff(roc_curve$FPR) * (roc_curve$TPR[-1] + roc_curve$TPR[-nrow(roc_curve)]) / 2)

    p5 <- ggplot(roc_curve, aes(x = FPR, y = TPR)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
      annotate("text", x = 0.7, y = 0.2, label = paste0("AUC = ", round(auc_val, 3)),
               size = 5, color = "steelblue") +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "Krzywa ROC", x = "False Positive Rate", y = "True Positive Rate") +
      base_theme
  } else if (length(num_x) >= 2) {
    library(ggcorrplot)
    corr_mat <- cor(spec$data[, num_x, drop = FALSE], use = "complete.obs")
    p5 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
                     colors = c("#6D9EC1", "white", "#E46726"), title = "Macierz korelacji cech") +
      base_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    p5 <- ggplot() + annotate("text", x = 1, y = 1, label = "Brak danych do wykresu", size = 5) + theme_void()
  }

  # ważność zmiennych (|koef| dla binarnej; proporcja błędów dla multiclass)
  if (k == 2) {
    coef_vals <- coef(model)[-1]
    coef_df   <- data.frame(
      Zmienna  = names(coef_vals),
      Wartosc  = abs(coef_vals)
    )
    p6 <- ggplot(coef_df, aes(x = reorder(Zmienna, Wartosc), y = Wartosc)) +
      geom_col(fill = "coral", alpha = 0.85) +
      coord_flip() +
      labs(title = "|Współczynniki| modelu", x = "Zmienna", y = "|Współczynnik|") +
      base_theme
  } else {
    coef_mat  <- coef(model)
    imp_vals  <- colMeans(abs(coef_mat[, -1, drop = FALSE]))
    coef_df   <- data.frame(Zmienna = names(imp_vals), Wartosc = imp_vals)
    p6 <- ggplot(coef_df, aes(x = reorder(Zmienna, Wartosc), y = Wartosc)) +
      geom_col(fill = "coral", alpha = 0.85) +
      coord_flip() +
      labs(title = "Średni |współczynnik| (multiclass)", x = "Zmienna", y = "Średni |wsp.|") +
      base_theme
  }

  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – Regresja logistyczna",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
}

implement_svm_classification <- function(spec) {
  library(e1071)

  data      <- spec$data
  target    <- spec$target_column
  x_cols    <- spec$x_columns
  train_pct <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8
  scaling   <- if (!is.null(spec$scaling_method))  spec$scaling_method  else "none"
  encoding  <- if (!is.null(spec$encoding_method)) spec$encoding_method else "onehot"
  balancing <- if (!is.null(spec$balancing_mode))  spec$balancing_mode  else "none"

  set.seed(123)
  n   <- nrow(data)
  idx <- sample(seq_len(n), size = floor(train_pct * n))
  train_raw <- data[idx,  , drop = FALSE]
  train     <- train_raw
  test  <- data[-idx, , drop = FALSE]

  # enkodowanie kolumn kategorycznych
  enc    <- safe_encode_x(train, test, x_cols = x_cols, encoding_method = encoding)
  train  <- enc$train
  test   <- enc$test
  x_cols <- enc$x_cols

  if (length(enc$text_cols) > 0)
    cat("Kolumny tekstowe zamienione na cechy numeryczne:",
        paste(enc$text_cols, collapse = ", "), "\n")

  num_x <- intersect(x_cols, names(train)[sapply(train, is.numeric)])

  scale_means <- NULL; scale_sds <- NULL; scale_mins <- NULL; scale_maxs <- NULL
  if (scaling == "standardization" && length(num_x) > 0) {
    scale_means <- sapply(train[, num_x, drop = FALSE], mean, na.rm = TRUE)
    scale_sds   <- sapply(train[, num_x, drop = FALSE], sd,   na.rm = TRUE)
    scale_sds[scale_sds == 0] <- 1
    train[, num_x] <- scale(train[, num_x, drop = FALSE], center = scale_means, scale = scale_sds)
    test[,  num_x] <- scale(test[,  num_x, drop = FALSE], center = scale_means, scale = scale_sds)
  } else if (scaling == "normalization" && length(num_x) > 0) {
    scale_mins <- sapply(train[, num_x, drop = FALSE], min, na.rm = TRUE)
    scale_maxs <- sapply(train[, num_x, drop = FALSE], max, na.rm = TRUE)
    rng <- scale_maxs - scale_mins; rng[rng == 0] <- 1
    train[, num_x] <- sweep(sweep(train[, num_x, drop = FALSE], 2, scale_mins, "-"), 2, rng, "/")
    test[,  num_x] <- sweep(sweep(test[,  num_x, drop = FALSE], 2, scale_mins, "-"), 2, rng, "/")
  }

  classes <- sort(unique(train[[target]]))
  train[[target]] <- factor(train[[target]], levels = classes)
  test[[target]]  <- factor(test[[target]],  levels = classes)

  class_weights_param <- NULL
  if (balancing == "class_weights") {
    tbl <- table(train[[target]])
    wts <- as.numeric(nrow(train) / (length(tbl) * tbl))
    names(wts) <- names(tbl)
    class_weights_param <- wts
  }

  formula <- as.formula(paste(
    paste0("factor(`", target, "`)"),
    "~",
    paste(paste0("`", x_cols, "`"), collapse = " + ")
  ))

  model <- svm(formula, data = train,
               kernel = "radial", cost = 1,
               class.weights = class_weights_param,
               scale = FALSE,        # skalowanie obsługiwane ręcznie wyżej
               probability = TRUE)

  pred_test <- predict(model, newdata = test)
  true_test <- test[[target]]
  cm        <- table(Prawdziwa = true_test, Przewidywana = pred_test)
  acc       <- sum(diag(cm)) / sum(cm)

  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0

  cat("====== OCENA MODELU ======\n")
  cat("Podział:", floor(train_pct * 100), "% trening /", round((1 - train_pct) * 100), "% test\n")
  cat("Skalowanie:", scaling, "| Balansowanie:", balancing, "\n\n")
  cat("Dokładność (Accuracy):", round(acc, 4), "\n\n")
  cat("Macierz pomyłek:\n"); print(cm); cat("\n")
  cat("Metryki per klasa:\n")
  print(data.frame(Precision = round(prec, 4), Recall = round(rec, 4), F1 = round(f1, 4)))

  cat("\n====== PARAMETRY MODELU ======\n")
  cat("Kernel: radial | Cost: 1\n")
  cat("Liczba support vectors:", model$tot.nSV, "\n")

  return(invisible(list(model = model, cm = cm, acc = acc, classes = as.character(classes),
                        test = test, pred_test = pred_test, true_test = true_test,
                        x_cols = x_cols, target = target, train_raw = train_raw,
                        scale_means = scale_means, scale_sds = scale_sds,
                        scale_mins = scale_mins, scale_maxs = scale_maxs)))



}

plot_svm_classification <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(e1071)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) return()

  res <- implement_svm_classification(spec)
  if (is.null(res)) return()

  cm         <- res$cm
  classes    <- res$classes
  true_test  <- res$true_test
  pred_test  <- res$pred_test
  x_cols     <- res$x_cols
  test_data  <- res$test
  model      <- res$model

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # macierz pomyłek
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Prawdziwa", "Przewidywana", "Liczba")

  p1 <- ggplot(cm_df, aes(x = Przewidywana, y = Prawdziwa, fill = Liczba)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Liczba), size = 6, fontface = "bold") +
    scale_fill_gradient(low = "#eaf4ff", high = "#2171b5") +
    labs(title = "Macierz pomyłek", x = "Przewidywana klasa", y = "Prawdziwa klasa") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # precision / Recall / F1
  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0
  metrics_long <- data.frame(
    Klasa   = rep(classes, 3),
    Metryka = rep(c("Precision", "Recall", "F1"), each = length(classes)),
    Wartosc = c(prec, rec, f1)
  )
  p2 <- ggplot(metrics_long, aes(x = Klasa, y = Wartosc, fill = Metryka)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Precision / Recall / F1", x = "Klasa", y = "Wartosc", fill = NULL) +
    base_theme + theme(legend.position = "bottom",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # scatter dwóch pierwszych cech z granicami decyzyjnymi (jeśli 2D)
  num_x <- intersect(x_cols, names(spec$data)[sapply(spec$data, is.numeric)])
  if (length(num_x) >= 2) {
    col1 <- num_x[1]; col2 <- num_x[2]
    scatter_df <- data.frame(
      X = test_data[[col1]],
      Y = test_data[[col2]],
      Klasa = true_test,
      Poprawna = ifelse(true_test == pred_test, "Tak", "Nie")
    )
    p3 <- ggplot(scatter_df, aes(x = X, y = Y, color = Klasa, shape = Poprawna)) +
      geom_point(size = 2.5, alpha = 0.75) +
      scale_shape_manual(values = c("Tak" = 16, "Nie" = 4)) +
      labs(title = paste0("Scatter: ", col1, " vs ", col2), x = col1, y = col2,
           color = "Klasa", shape = "Poprawna_predykcja") +
      base_theme + theme(legend.position = "bottom")
  } else {
    p3 <- ggplot() + annotate("text", x = 1, y = 1, label = "Za mało zmiennych numerycznych\ndo wykresu scatter", size = 5) + theme_void()
  }

  # rozkład klas (zbiór testowy)
  class_dist <- as.data.frame(table(Klasa = true_test))
  p4 <- ggplot(class_dist, aes(x = Klasa, y = Freq, fill = Klasa)) +
    geom_col(alpha = 0.85) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Rozkład klas (test)", x = "Klasa", y = "Liczba obserwacji") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # support vectors per klasa
  sv_counts <- table(factor(model$fitted[model$index], levels = classes))
  sv_df <- data.frame(Klasa = names(sv_counts), SV = as.integer(sv_counts))
  p5 <- ggplot(sv_df, aes(x = Klasa, y = SV, fill = Klasa)) +
    geom_col(alpha = 0.85) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Support vectors per klasa", x = "Klasa", y = "Liczba SV") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # macierz korelacji cech numerycznych
  if (length(num_x) >= 2) {
    library(ggcorrplot)
    corr_mat <- cor(spec$data[, num_x, drop = FALSE], use = "complete.obs")
    p6 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
                     colors = c("#6D9EC1", "white", "#E46726"), title = "Macierz korelacji cech") +
      base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    p6 <- ggplot() + annotate("text", x = 1, y = 1,
                               label = "Za mało zmiennych numerycznych\ndo macierzy korelacji", size = 5) + theme_void()
  }

  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – SVM (klasyfikacja)",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
}

implement_rf_classification <- function(spec) {
  library(randomForest)

  data      <- spec$data
  target    <- spec$target_column
  x_cols    <- spec$x_columns
  train_pct <- if (!is.null(spec$train_percent)) spec$train_percent / 100 else 0.8
  scaling   <- if (!is.null(spec$scaling_method))  spec$scaling_method  else "none"
  encoding  <- if (!is.null(spec$encoding_method)) spec$encoding_method else "onehot"
  balancing <- if (!is.null(spec$balancing_mode))  spec$balancing_mode  else "none"

  set.seed(123)
  n   <- nrow(data)
  idx <- sample(seq_len(n), size = floor(train_pct * n))
  train_raw <- data[idx,  , drop = FALSE]
  train     <- train_raw
  test  <- data[-idx, , drop = FALSE]

  # enkodowanie kolumn kategorycznych
  enc    <- safe_encode_x(train, test, x_cols = x_cols, encoding_method = encoding)
  train  <- enc$train
  test   <- enc$test
  x_cols <- enc$x_cols

  if (length(enc$text_cols) > 0)
    cat("Kolumny tekstowe zamienione na cechy numeryczne:",
        paste(enc$text_cols, collapse = ", "), "\n")

  num_x <- intersect(x_cols, names(train)[sapply(train, is.numeric)])
  scale_means <- NULL; scale_sds <- NULL; scale_mins <- NULL; scale_maxs <- NULL


  if (scaling == "standardization" && length(num_x) > 0) {
    means <- sapply(train[, num_x, drop = FALSE], mean, na.rm = TRUE)
    sds   <- sapply(train[, num_x, drop = FALSE], sd,   na.rm = TRUE)
    sds[sds == 0] <- 1
    scale_means <- means; scale_sds <- sds
    train[, num_x] <- scale(train[, num_x, drop = FALSE], center = means, scale = sds)
    test[,  num_x] <- scale(test[,  num_x, drop = FALSE], center = means, scale = sds)
  } else if (scaling == "normalization" && length(num_x) > 0) {
    mins  <- sapply(train[, num_x, drop = FALSE], min, na.rm = TRUE)
    maxs  <- sapply(train[, num_x, drop = FALSE], max, na.rm = TRUE)
    rng   <- maxs - mins; rng[rng == 0] <- 1
    scale_mins <- mins; scale_maxs <- maxs
    train[, num_x] <- sweep(sweep(train[, num_x, drop = FALSE], 2, mins, "-"), 2, rng, "/")
    test[,  num_x] <- sweep(sweep(test[,  num_x, drop = FALSE], 2, mins, "-"), 2, rng, "/")
  }

  classes <- sort(unique(train[[target]]))
  train[[target]] <- factor(train[[target]], levels = classes)
  test[[target]]  <- factor(test[[target]],  levels = classes)

  class_wts <- NULL
  if (balancing == "class_weights") {
    tbl <- table(train[[target]])
    wts <- as.numeric(nrow(train) / (length(tbl) * tbl))
    names(wts) <- names(tbl)
    class_wts <- wts
  }

  formula <- as.formula(paste(
    paste0("factor(`", target, "`)"),
    "~",
    paste(paste0("`", x_cols, "`"), collapse = " + ")
  ))

  model <- randomForest(formula, data = train,
                        ntree = 500, importance = TRUE,
                        classwt = class_wts)

  pred_test <- predict(model, newdata = test)
  true_test <- test[[target]]
  cm        <- table(Prawdziwa = true_test, Przewidywana = pred_test)
  acc       <- sum(diag(cm)) / sum(cm)

  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0

  cat("====== OCENA MODELU ======\n")
  cat("Podział:", floor(train_pct * 100), "% trening /", round((1 - train_pct) * 100), "% test\n")
  cat("Skalowanie:", scaling, "| Balansowanie:", balancing, "\n\n")
  cat("Dokładność (Accuracy):", round(acc, 4), "\n")
  cat("OOB Error Rate:", round(model$err.rate[nrow(model$err.rate), "OOB"], 4), "\n\n")
  cat("Macierz pomyłek:\n"); print(cm); cat("\n")
  cat("Metryki per klasa:\n")
  print(data.frame(Precision = round(prec, 4), Recall = round(rec, 4), F1 = round(f1, 4)))
  cat("\n====== WAŻNOŚĆ ZMIENNYCH ======\n")
  imp <- importance(model)
  for (i in rownames(imp)) cat(i, ":", round(imp[i, 1], 3), "\n")

  return(invisible(list(model = model, cm = cm, acc = acc, classes = as.character(classes),
                        test = test, pred_test = pred_test, true_test = true_test,
                        x_cols = x_cols, target = target, train_raw = train_raw,
                        scale_means = scale_means, scale_sds = scale_sds,
                        scale_mins = scale_mins, scale_maxs = scale_maxs)))



}

plot_rf_classification <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(randomForest)

  if (is.null(spec$target_column)) return()

  res <- implement_rf_classification(spec)
  if (is.null(res)) return()

  model      <- res$model
  cm         <- res$cm
  classes    <- res$classes
  true_test  <- res$true_test
  pred_test  <- res$pred_test
  x_cols     <- res$x_cols

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # macierz pomyłek
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Prawdziwa", "Przewidywana", "Liczba")
  p1 <- ggplot(cm_df, aes(x = Przewidywana, y = Prawdziwa, fill = Liczba)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Liczba), size = 6, fontface = "bold") +
    scale_fill_gradient(low = "#eaf4ff", high = "#2171b5") +
    labs(title = "Macierz pomyłek", x = "Przewidywana klasa", y = "Prawdziwa klasa") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # precision / Recall / F1
  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  prec[is.nan(prec)] <- 0; rec[is.nan(rec)] <- 0; f1[is.nan(f1)] <- 0
  metrics_long <- data.frame(
    Klasa   = rep(classes, 3),
    Metryka = rep(c("Precision", "Recall", "F1"), each = length(classes)),
    Wartosc = c(prec, rec, f1)
  )
  p2 <- ggplot(metrics_long, aes(x = Klasa, y = Wartosc, fill = Metryka)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Precision / Recall / F1", x = "Klasa", y = "Wartosc", fill = NULL) +
    base_theme + theme(legend.position = "bottom",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # ważność zmiennych (MeanDecreaseGini)
  imp     <- as.data.frame(importance(model))
  imp$var <- rownames(imp)
  imp_col <- if ("MeanDecreaseGini" %in% colnames(imp)) "MeanDecreaseGini" else colnames(imp)[1]
  p3 <- ggplot(imp, aes(x = reorder(var, .data[[imp_col]]), y = .data[[imp_col]])) +
    geom_col(fill = "coral", alpha = 0.85) +
    coord_flip() +
    labs(title = paste0("Ważność zmiennych (", imp_col, ")"),
         x = "Zmienna", y = imp_col) +
    base_theme

  # oOB error vs liczba drzew
  oob_df <- as.data.frame(model$err.rate)
  oob_df$ntree <- seq_len(nrow(oob_df))
  p4 <- ggplot(oob_df, aes(x = ntree, y = OOB)) +
    geom_line(color = "darkgreen", linewidth = 0.8) +
    labs(title = "Błąd OOB vs liczba drzew", x = "Liczba drzew", y = "OOB Error Rate") +
    base_theme

  # rozkład klas (zbiór testowy)
  class_dist <- as.data.frame(table(Klasa = true_test))
  p5 <- ggplot(class_dist, aes(x = Klasa, y = Freq, fill = Klasa)) +
    geom_col(alpha = 0.85) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Rozkład klas (test)", x = "Klasa", y = "Liczba obserwacji") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # macierz korelacji cech numerycznych
  num_x <- intersect(x_cols, names(spec$data)[sapply(spec$data, is.numeric)])
  if (length(num_x) >= 2) {
    library(ggcorrplot)
    corr_mat <- cor(spec$data[, num_x, drop = FALSE], use = "complete.obs")
    p6 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
                     colors = c("#6D9EC1", "white", "#E46726"), title = "Macierz korelacji cech") +
      base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    p6 <- ggplot() + annotate("text", x = 1, y = 1,
                               label = "Za mało zmiennych numerycznych\ndo macierzy korelacji", size = 5) + theme_void()
  }

  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – Random Forest (klasyfikacja)",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
}

implement_kmeans_clustering <- function(spec) {
  library(cluster)

  data    <- spec$data
  cols    <- spec$columns
  k       <- if (!is.null(spec$extra_params[["Liczba klastrów"]])) spec$extra_params[["Liczba klastrów"]] else 3

  if (is.null(cols) || length(cols) == 0) {
    cat("Brak wybranych kolumn.\n"); return(invisible(NULL))
  }

  df <- data[, cols, drop = FALSE]
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  df <- na.omit(df)

  if (nrow(df) < k) {
    cat("Za mało obserwacji dla k =", k, "\n"); return(invisible(NULL))
  }

  set.seed(123)
  model <- kmeans(scale(df), centers = k, nstart = 25, iter.max = 100)

  # wskaźniki jakości
  sil   <- silhouette(model$cluster, dist(scale(df)))
  sil_avg <- mean(sil[, 3])

  wcss <- model$tot.withinss
  bss  <- model$betweenss
  tss  <- model$totss

  cat("====== OCENA MODELU K-MEANS ======\n")
  cat("Liczba klastrów (k):", k, "\n")
  cat("Liczba obserwacji:", nrow(df), "\n\n")
  cat("WCSS (Within-cluster SS):", round(wcss, 3), "\n")
  cat("BSS  (Between-cluster SS):", round(bss, 3), "\n")
  cat("TSS  (Total SS):", round(tss, 3), "\n")
  cat("BSS/TSS:", round(bss / tss, 4), "(im bliżej 1, tym lepszy podział)\n\n")
  cat("Średnia szerokość sylwetki:", round(sil_avg, 4),
      ifelse(sil_avg > 0.5, " [dobra struktura]",
             ifelse(sil_avg > 0.25, " [słaba struktura]", " [brak wyraźnej struktury]")), "\n\n")
  cat("====== LICZEBNOŚĆ KLASTRÓW ======\n")
  print(table(Klaster = model$cluster))

  return(invisible(list(model = model, df_scaled = scale(df), df = df,
                        k = k, sil = sil, sil_avg = sil_avg, cols = cols)))
}

plot_kmeans_clustering <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(cluster)

  if (is.null(spec$data) || nrow(spec$data) == 0) return()

  res <- implement_kmeans_clustering(spec)
  if (is.null(res)) return()

  model     <- res$model
  df        <- res$df
  df_scaled <- res$df_scaled
  k         <- res$k
  sil       <- res$sil
  sil_avg   <- res$sil_avg
  cols      <- res$cols

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  cluster_fac <- factor(model$cluster)

  # scatter dwóch pierwszych zmiennych
  col1 <- cols[1]; col2 <- if (length(cols) >= 2) cols[2] else cols[1]
  scatter_df <- data.frame(X = df[[col1]], Y = df[[col2]], Klaster = cluster_fac)
  # centroidy odskalowane do oryginalnej skali
  centers_df <- as.data.frame(model$centers %*% diag(apply(df, 2, sd)) +
                               matrix(colMeans(df), nrow = k, ncol = ncol(df), byrow = TRUE))
  names(centers_df) <- names(df)

  p1 <- ggplot(scatter_df, aes(x = X, y = Y, color = Klaster)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_point(data = data.frame(X = centers_df[[col1]], Y = centers_df[[col2]],
                                  Klaster = factor(seq_len(k))),
               size = 5, shape = 8, color = "black") +
    labs(title = paste0("Klastry: ", col1, " vs ", col2),
         x = col1, y = col2, color = "Klaster") +
    base_theme + theme(legend.position = "bottom")

  # metoda łokcia (WCSS dla k = 1..10)
  max_k <- min(10, nrow(df) - 1)
  wcss_vals <- sapply(1:max_k, function(ki) {
    set.seed(123)
    kmeans(df_scaled, centers = ki, nstart = 10, iter.max = 50)$tot.withinss
  })
  elbow_df <- data.frame(K = 1:max_k, WCSS = wcss_vals)
  p2 <- ggplot(elbow_df, aes(x = K, y = WCSS)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2.5) +
    geom_vline(xintercept = k, linetype = "dashed", color = "red", linewidth = 0.8) +
    scale_x_continuous(breaks = 1:max_k) +
    labs(title = "Metoda łokcia (Elbow)", x = "Liczba klastrów k", y = "WCSS") +
    base_theme

  # wykres sylwetki
  sil_df <- data.frame(
    obs     = seq_len(nrow(sil)),
    width   = sil[, 3],
    cluster = factor(sil[, 1])
  )
  sil_df <- sil_df[order(sil_df$cluster, -sil_df$width), ]
  sil_df$obs <- seq_len(nrow(sil_df))
  p3 <- ggplot(sil_df, aes(x = obs, y = width, fill = cluster)) +
    geom_col(width = 1, alpha = 0.8) +
    geom_hline(yintercept = sil_avg, linetype = "dashed", color = "red") +
    annotate("text", x = nrow(sil_df) * 0.8, y = sil_avg + 0.05,
             label = paste0("Avg = ", round(sil_avg, 3)), color = "red", size = 4) +
    labs(title = "Wykres sylwetki", x = "Obserwacja", y = "Szerokosc sylwetki", fill = "Klaster") +
    base_theme + theme(legend.position = "bottom")

  # liczebność klastrów
  cnt_df <- as.data.frame(table(Klaster = cluster_fac))
  p4 <- ggplot(cnt_df, aes(x = Klaster, y = Freq, fill = Klaster)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = Freq), vjust = -0.4, size = 4.5) +
    labs(title = "Liczebność klastrów", x = "Klaster", y = "Liczba obserwacji") +
    base_theme + theme(legend.position = "none")

  # heatmapa centroidów per klaster
  centers_long <- reshape(
    as.data.frame(model$centers),
    varying   = colnames(model$centers),
    v.names   = "Wartosc",
    timevar   = "Cecha",
    times     = colnames(model$centers),
    direction = "long"
  )
  centers_long$Klaster <- factor(rep(seq_len(k), ncol(model$centers)))
  p5 <- ggplot(centers_long, aes(x = Cecha, y = Klaster, fill = Wartosc)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Wartosc, 2)), size = 3.5) +
    scale_fill_gradient2(low = "#6D9EC1", mid = "white", high = "#E46726", midpoint = 0) +
    labs(title = "Centroidy klastrów (standaryzowane)", x = "Cecha", y = "Klaster") +
    base_theme + theme(axis.text.x = element_text(angle = 40, hjust = 1))

  # średnia sylwetka per klaster
  sil_per_k <- tapply(sil[, 3], sil[, 1], mean)
  sil_k_df  <- data.frame(Klaster = factor(names(sil_per_k)), Sylwetka = as.numeric(sil_per_k))
  p6 <- ggplot(sil_k_df, aes(x = Klaster, y = Sylwetka, fill = Klaster)) +
    geom_col(alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ylim(-1, 1) +
    labs(title = "Średnia sylwetka per klaster", x = "Klaster", y = "Średnia szerokość sylwetki") +
    base_theme + theme(legend.position = "none")

  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – K-means",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
}

implement_dbscan_clustering <- function(spec) {
  library(dbscan)
  library(cluster)

  data <- spec$data
  cols <- spec$columns

  if (is.null(cols) || length(cols) == 0) {
    cat("Brak wybranych kolumn.\n"); return(invisible(NULL))
  }

  df <- data[, cols, drop = FALSE]
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  df <- na.omit(df)

  df_scaled <- scale(df)

  # minPts z inputu (domyślnie 5)
  minPts   <- if (!is.null(spec$extra_params[["minPts"]])) as.integer(spec$extra_params[["minPts"]]) else 5
  minPts   <- max(2L, minPts)

  knn_dist <- sort(kNNdist(df_scaled, k = minPts - 1))
  # eps ~ 90. percentyl dystansu k-NN
  eps_auto <- quantile(knn_dist, 0.9)

  model <- dbscan(df_scaled, eps = eps_auto, minPts = minPts)

  n_clusters <- length(unique(model$cluster[model$cluster != 0]))
  n_noise    <- sum(model$cluster == 0)
  n_obs      <- nrow(df)

  cat("====== OCENA MODELU DBSCAN ======\n")
  cat("Parametry: eps =", round(eps_auto, 4), "| minPts =", minPts, "\n")
  cat("(eps dobrane automatycznie z k-NN distance plot)\n\n")
  cat("Liczba wykrytych klastrów:", n_clusters, "\n")
  cat("Liczba punktów szumu (noise):", n_noise,
      paste0("(", round(100 * n_noise / n_obs, 1), "%)"), "\n")
  cat("Łączna liczba obserwacji:", n_obs, "\n\n")

  if (n_clusters > 0) {
    cat("====== LICZEBNOŚĆ KLASTRÓW ======\n")
    tbl <- table(Klaster = model$cluster)
    print(tbl)

    non_noise <- model$cluster != 0
    if (sum(non_noise) > 1 && n_clusters > 1) {
      sil <- silhouette(model$cluster[non_noise],
                        dist(df_scaled[non_noise, , drop = FALSE]))
      sil_avg <- mean(sil[, 3])
      cat("\nŚrednia szerokość sylwetki (bez szumu):", round(sil_avg, 4), "\n")
    }
  } else {
    cat("Brak klastrów – spróbuj zmienić parametry.\n")
  }

  return(invisible(list(model = model, df = df, df_scaled = df_scaled,
                        eps = eps_auto, minPts = minPts,
                        n_clusters = n_clusters, n_noise = n_noise, cols = cols,
                        knn_dist = knn_dist)))
}

plot_dbscan_clustering <- function(spec) {
  library(ggplot2)
  library(patchwork)
  library(dbscan)
  library(cluster)

  if (is.null(spec$data) || length(spec$columns) == 0) return()

  res <- implement_dbscan_clustering(spec)
  if (is.null(res)) return()

  model     <- res$model
  df        <- res$df
  df_scaled <- res$df_scaled
  cols      <- res$cols
  eps       <- res$eps
  minPts    <- res$minPts
  knn_dist  <- res$knn_dist
  n_clusters <- res$n_clusters

  # etykiety: 0 = szum
  cluster_labels <- ifelse(model$cluster == 0, "Szum", paste0("Klaster ", model$cluster))
  cluster_fac    <- factor(cluster_labels)
  palette_vals   <- c("Szum" = "grey70",
                      setNames(scales::hue_pal()(n_clusters),
                               paste0("Klaster ", seq_len(n_clusters))))

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  col1 <- cols[1]; col2 <- if (length(cols) >= 2) cols[2] else cols[1]

  # scatter klastrów
  scatter_df <- data.frame(X = df[[col1]], Y = df[[col2]], Klaster = cluster_fac)
  p1 <- ggplot(scatter_df, aes(x = X, y = Y, color = Klaster)) +
    geom_point(size = 2.5, alpha = 0.75) +
    scale_color_manual(values = palette_vals, na.value = "grey70") +
    labs(title = paste0("Klastry DBSCAN: ", col1, " vs ", col2),
         x = col1, y = col2, color = NULL) +
    base_theme + theme(legend.position = "bottom")

  # k-NN distance plot do doboru eps
  knn_df <- data.frame(idx = seq_along(knn_dist), dist = knn_dist)
  p2 <- ggplot(knn_df, aes(x = idx, y = dist)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = eps, linetype = "dashed", color = "red", linewidth = 0.9) +
    annotate("text", x = nrow(knn_df) * 0.7, y = eps * 1.08,
             label = paste0("eps = ", round(eps, 3)), color = "red", size = 4) +
    labs(title = paste0("k-NN Distance Plot (k=", minPts - 1, ")"),
         x = "Posortowane obserwacje", y = "Odleglosc do k-tego sąsiada") +
    base_theme

  # liczebność klastrów (z szumem)
  cnt_df <- as.data.frame(table(Klaster = cluster_fac))
  cnt_df$Kolor <- ifelse(cnt_df$Klaster == "Szum", "grey70", "steelblue")
  p3 <- ggplot(cnt_df, aes(x = reorder(Klaster, -Freq), y = Freq, fill = Klaster)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = Freq), vjust = -0.4, size = 4) +
    scale_fill_manual(values = palette_vals) +
    labs(title = "Liczebność klastrów", x = "Klaster", y = "Liczba obserwacji") +
    base_theme + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 30, hjust = 1))

  # wykres sylwetki (bez szumu)
  non_noise <- model$cluster != 0
  if (sum(non_noise) > 2 && n_clusters >= 2) {
    sil <- silhouette(model$cluster[non_noise],
                      dist(df_scaled[non_noise, , drop = FALSE]))
    sil_avg <- mean(sil[, 3])
    sil_df <- data.frame(
      obs     = seq_len(nrow(sil)),
      width   = sil[, 3],
      cluster = factor(sil[, 1])
    )
    sil_df <- sil_df[order(sil_df$cluster, -sil_df$width), ]
    sil_df$obs <- seq_len(nrow(sil_df))
    p4 <- ggplot(sil_df, aes(x = obs, y = width, fill = cluster)) +
      geom_col(width = 1, alpha = 0.8) +
      geom_hline(yintercept = sil_avg, linetype = "dashed", color = "red") +
      annotate("text", x = nrow(sil_df) * 0.8, y = sil_avg + 0.05,
               label = paste0("Avg = ", round(sil_avg, 3)), color = "red", size = 4) +
      labs(title = "Wykres sylwetki (bez szumu)", x = "Obserwacja",
           y = "Szerokosc sylwetki", fill = "Klaster") +
      base_theme + theme(legend.position = "bottom")
  } else {
    p4 <- ggplot() +
      annotate("text", x = 1, y = 1,
               label = "Za mało klastrów\ndo wykresu sylwetki", size = 5) +
      theme_void() + ggtitle("Wykres sylwetki")
  }

  # histogram odległości punktów od centroidu klastra
  if (n_clusters > 0) {
    centers <- do.call(rbind, lapply(seq_len(n_clusters), function(ci) {
      pts <- df_scaled[model$cluster == ci, , drop = FALSE]
      colMeans(pts)
    }))
    dists_to_center <- sapply(seq_len(nrow(df_scaled)), function(i) {
      cl <- model$cluster[i]
      if (cl == 0) return(NA)
      sqrt(sum((df_scaled[i, ] - centers[cl, ])^2))
    })
    dist_df <- data.frame(
      Odleglosc = dists_to_center[!is.na(dists_to_center)],
      Klaster   = factor(model$cluster[model$cluster != 0])
    )
    p5 <- ggplot(dist_df, aes(x = Odleglosc, fill = Klaster)) +
      geom_histogram(bins = 25, alpha = 0.7, position = "identity", color = "white") +
      labs(title = "Odległości punktów od centroidu klastra",
           x = "Odleglosc euklidesowa", y = "Liczba punktów", fill = "Klaster") +
      base_theme + theme(legend.position = "bottom")
  } else {
    p5 <- ggplot() + annotate("text", x = 1, y = 1, label = "Brak klastrów", size = 5) + theme_void()
  }

  # udział szumu w danych
  noise_pct   <- round(100 * res$n_noise / nrow(df), 1)
  cluster_pct <- 100 - noise_pct
  pie_df <- data.frame(
    Kategoria = c("Klastrowane", "Szum"),
    Procent   = c(cluster_pct, noise_pct)
  )
  p6 <- ggplot(pie_df, aes(x = "", y = Procent, fill = Kategoria)) +
    geom_col(width = 1, alpha = 0.85) +
    coord_polar("y") +
    geom_text(aes(label = paste0(Procent, "%")),
              position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
    scale_fill_manual(values = c("Klastrowane" = "steelblue", "Szum" = "grey70")) +
    labs(title = "Udział szumu w danych", fill = NULL) +
    base_theme + theme(axis.text = element_blank(), axis.title = element_blank(),
                       panel.grid = element_blank(), legend.position = "bottom")

  (p1 | p2 | p3) /
    (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – DBSCAN",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
}

implement_hclust_clustering <- function(spec) {
  data <- spec$data
  cols <- spec$columns

  if (is.null(cols) || length(cols) == 0) {
    cat("Brak wybranych kolumn.\n"); return(invisible(NULL))
  }

  df   <- data[, cols, drop = FALSE]
  df   <- df[, sapply(df, is.numeric), drop = FALSE]
  df   <- na.omit(df)
  cols <- names(df)

  if (nrow(df) < 2) { cat("Za mało obserwacji.\n"); return(invisible(NULL)) }

  df_sc <- scale(df)
  df_sc_df <- as.data.frame(df_sc)
  colnames(df_sc_df) <- cols

  method <- if (!is.null(spec$extra_params[["method"]])) spec$extra_params[["method"]] else "ward.D2"
  k      <- if (!is.null(spec$extra_params[["k_hclust"]])) as.integer(spec$extra_params[["k_hclust"]]) else 3L
  k      <- max(2L, min(k, nrow(df) - 1L))

  hc     <- hclust(dist(df_sc), method = method)
  labels <- cutree(hc, k = k)

  n_obs      <- nrow(df)
  n_clusters <- length(unique(labels))

  cat("====== OCENA MODELU – HCLUST ======\n")
  cat("Metoda łączenia:", method, "\n")
  cat("Liczba klastrów:", k, "\n")
  cat("Liczba obserwacji:", n_obs, "\n\n")
  cat("====== LICZEBNOŚĆ KLASTRÓW ======\n")
  print(table(Klaster = labels))

  if (n_clusters > 1 && n_obs <= 5000) {
    sil     <- cluster::silhouette(labels, dist(df_sc))
    sil_avg <- mean(sil[, 3])
    cat("\nŚrednia szerokość sylwetki:", round(sil_avg, 4), "\n")
  }

  return(invisible(list(
    labels = labels, df = df, df_sc = df_sc_df,
    hc = hc, k = k, method = method,
    n_clusters = n_clusters, cols = cols
  )))
}

plot_hclust_clustering <- function(spec) {
  library(ggplot2)
  library(patchwork)

  if (is.null(spec$data) || length(spec$columns) == 0) return()
  res <- implement_hclust_clustering(spec)
  if (is.null(res)) return()

  labels      <- res$labels
  df          <- res$df
  df_sc       <- res$df_sc      # data.frame, colnames == cols
  cols        <- res$cols
  hc          <- res$hc
  k           <- res$k
  n_clusters  <- res$n_clusters
  cluster_fac <- factor(labels)
  col1 <- cols[1]
  col2 <- if (length(cols) >= 2) cols[2] else cols[1]

  base_theme <- theme_minimal(base_size = 13) +
    theme(plot.title  = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title  = element_text(size = 11),
          axis.text   = element_text(size = 10))

  # --- centroidy ---
  kl_sorted   <- sort(unique(labels))
  cen_sc_list <- lapply(kl_sorted, function(kl) colMeans(df_sc[labels == kl, cols, drop = FALSE]))
  cen_sc_mat  <- do.call(rbind, cen_sc_list)
  colnames(cen_sc_mat) <- cols

  sd_v   <- apply(df[, cols, drop = FALSE], 2, sd); sd_v[sd_v == 0] <- 1
  mean_v <- colMeans(df[, cols, drop = FALSE])
  cen_orig        <- as.data.frame(t(apply(cen_sc_mat, 1, function(r) r * sd_v + mean_v)))
  colnames(cen_orig) <- cols

  # p1 – scatter
  scatter_df <- data.frame(X = df[[col1]], Y = df[[col2]], Klaster = cluster_fac)
  cen_df     <- data.frame(X = cen_orig[[col1]], Y = cen_orig[[col2]])
  p1 <- ggplot(scatter_df, aes(x = X, y = Y, color = Klaster)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_point(data = cen_df, aes(x = X, y = Y), inherit.aes = FALSE,
               color = "black", size = 5, shape = 8) +
    labs(title = paste0(col1, " vs ", col2), x = col1, y = col2, color = "Klaster") +
    base_theme + theme(legend.position = "bottom")

  # p2 – dendrogram (base R, konwersja na ggplot przez ggdendro segmenty ręcznie)
  n_show  <- min(nrow(df), 60)
  hc_show <- if (nrow(df) > n_show) {
    set.seed(42)
    idx <- sample(seq_len(nrow(df)), n_show)
    hclust(dist(df_sc[idx, cols, drop = FALSE]), method = res$method)
  } else hc

  # wyciągnij segmenty z merge/height
  build_segs <- function(hc_obj) {
    m <- hc_obj$merge; h <- hc_obj$height; n <- length(hc_obj$order)
    xpos <- numeric(n); xpos[hc_obj$order] <- seq_len(n)
    nx   <- numeric(nrow(m) + n); nx[seq_len(n)] <- xpos
    segs <- vector("list", 3 * nrow(m))
    si   <- 1L
    for (i in seq_len(nrow(m))) {
      li <- m[i,1]; ri <- m[i,2]
      lx <- if (li<0) xpos[-li] else nx[n+li]
      rx <- if (ri<0) xpos[-ri] else nx[n+ri]
      ly <- if (li<0) 0         else h[li]
      ry <- if (ri<0) 0         else h[ri]
      cx <- (lx+rx)/2; nx[n+i] <- cx
      segs[[si]]   <- c(lx,ly,lx,h[i]);   si <- si+1L
      segs[[si]]   <- c(rx,ry,rx,h[i]);   si <- si+1L
      segs[[si]]   <- c(lx,h[i],rx,h[i]); si <- si+1L
    }
    s <- as.data.frame(do.call(rbind, segs[seq_len(si-1)]))
    names(s) <- c("x","y","xend","yend")
    cut_h <- if (k>=2 && k<=nrow(m)) { sh <- sort(h,decreasing=TRUE); (sh[k-1]+sh[k])/2 } else max(h)*0.5
    list(segs=s, cut_h=cut_h)
  }
  dend <- build_segs(hc_show)
  p2 <- ggplot(dend$segs, aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(color="grey40") +
    geom_hline(yintercept=dend$cut_h, linetype="dashed", color="red", linewidth=0.7) +
    labs(title="Dendrogram (próbka)", x="", y="Wysokość") +
    base_theme + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

  # p3 – liczebność
  cnt_df <- as.data.frame(table(Klaster = cluster_fac))
  p3 <- ggplot(cnt_df, aes(x=Klaster, y=Freq, fill=Klaster)) +
    geom_col(alpha=0.85) +
    geom_text(aes(label=Freq), vjust=-0.4, size=4.5) +
    labs(title="Liczebność klastrów", x="Klaster", y="Liczba obserwacji") +
    base_theme + theme(legend.position="none")

  # p4 – sylwetka
  if (n_clusters > 1 && nrow(df) <= 5000) {
    sil     <- cluster::silhouette(labels, dist(as.matrix(df_sc[, cols])))
    sil_avg <- mean(sil[,3])
    sil_df  <- data.frame(obs=seq_len(nrow(sil)), width=sil[,3], cluster=factor(sil[,1]))
    sil_df  <- sil_df[order(sil_df$cluster, -sil_df$width), ]; sil_df$obs <- seq_len(nrow(sil_df))
    p4 <- ggplot(sil_df, aes(x=obs, y=width, fill=cluster)) +
      geom_col(width=1, alpha=0.8) +
      geom_hline(yintercept=sil_avg, linetype="dashed", color="red") +
      annotate("text", x=nrow(sil_df)*0.75, y=sil_avg+0.05,
               label=paste0("Avg=",round(sil_avg,3)), color="red", size=4) +
      labs(title="Wykres sylwetki", x="Obserwacja", y="Szerokość", fill="Klaster") +
      base_theme + theme(legend.position="bottom")
  } else {
    p4 <- ggplot() + annotate("text",x=1,y=1,label="Sylwetka niedostępna",size=5) +
      theme_void() + ggtitle("Wykres sylwetki")
  }

  # p5 – heatmapa centroidów (skalowanych)
  cen_long <- do.call(rbind, lapply(seq_along(cols), function(ci) {
    data.frame(Klaster  = factor(kl_sorted),
               Zmienna  = cols[ci],
               Wartosc  = cen_sc_mat[, ci])
  }))
  p5 <- ggplot(cen_long, aes(x=Zmienna, y=Klaster, fill=Wartosc)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="#6D9EC1", mid="white", high="#E46726", midpoint=0) +
    labs(title="Centroidy (skalowane)", x="Zmienna", y="Klaster") +
    base_theme + theme(axis.text.x=element_text(angle=35, hjust=1))

  # p6 – boxplot
  box_df <- data.frame(df[, cols, drop=FALSE], Klaster=cluster_fac)
  p6 <- ggplot(box_df, aes(x=Klaster, y=.data[[col1]], fill=Klaster)) +
    geom_boxplot(alpha=0.75, outlier.alpha=0.4) +
    labs(title=paste0("Rozkład ", col1), x="Klaster", y=col1) +
    base_theme + theme(legend.position="none")

  (p1 | p2 | p3) / (p4 | p5 | p6) +
    plot_annotation(
      title = "Diagnostyka – klasteryzacja hierarchiczna (hclust)",
      theme = theme(plot.title = element_text(size=18, face="bold", hjust=0.5))
    )
}

print_model_spec <- function(spec) {
  cat("========== KONFIGURACJA MODELU ==========\n")
  for (key in names(spec)) {
    value <- spec[[key]]

    if (is.data.frame(value)) {
      cat(key, ": data.frame (wiersze=", nrow(value), ", kolumny=", ncol(value), ")\n", sep = "")
      next
    }

    if (is.atomic(value) && length(value) > 1) {
      cat(key, ": ", paste(value, collapse = ", "), "\n", sep = "")
      next
    }

    if (is.atomic(value)) {
      cat(key, ": ", value, "\n", sep = "")
      next
    }

    cat(key, ":\n", sep = "")
    print(value)
  }
  cat("=== end ===")
}
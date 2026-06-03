afe_encode_x <- function(train, test = NULL, new_obs = NULL,
                         x_cols, max_cat = 20) {

  encodings <- list()   # mapowania kategorii na liczby
  text_cols <- c()      # kolumny tekstowe zamienione na nowe cechy

  # tworzy podstawowe cechy opisujące tekst
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

    names(result) <- paste0(
      col,
      c("__nchar", "__nwords", "__ndigits", "__nupper", "__nspecial")
    )

    result
  }

  for (col in x_cols) {
    col_data <- train[[col]]

    # kolumny numeryczne nie wymagają kodowania
    if (is.numeric(col_data)) next

    uniq <- unique(na.omit(as.character(col_data)))

    if (length(uniq) <= max_cat) {
      # kodowanie kolumny kategorycznej
      lvls <- sort(uniq)
      encodings[[col]] <- lvls

      encode_col <- function(v, lvls) {
        v <- as.character(v)

        # nieznane wartości otrzymują 0
        ifelse(v %in% lvls, match(v, lvls), 0L)
      }

      train[[col]] <- encode_col(train[[col]], lvls)

      if (!is.null(test)) {
        test[[col]] <- encode_col(test[[col]], lvls)
      }

      if (!is.null(new_obs)) {
        new_obs[[col]] <- encode_col(new_obs[[col]], lvls)
      }

    } else {
      # dla długich tekstów tworzone są cechy pomocnicze
      text_cols <- c(text_cols, col)

      train <- cbind(
        train[, setdiff(names(train), col), drop = FALSE],
        text_features(train, col)
      )

      if (!is.null(test)) {
        test <- cbind(
          test[, setdiff(names(test), col), drop = FALSE],
          text_features(test, col)
        )
      }

      if (!is.null(new_obs)) {
        new_obs <- cbind(
          new_obs[, setdiff(names(new_obs), col), drop = FALSE],
          text_features(new_obs, col)
        )
      }
    }
  }

  # aktualizacja listy zmiennych po dodaniu cech tekstowych
  new_x_cols <- x_cols

  for (col in text_cols) {
    new_x_cols <- setdiff(new_x_cols, col)

    new_x_cols <- c(
      new_x_cols,
      paste0(col, c("__nchar", "__nwords", "__ndigits", "__nupper", "__nspecial"))
    )
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

# budowanie specyfikacji
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
    show_meanshift_config = implement_meanshift_clustering(spec),
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
    # show_logistic_config = plot_logistic_classification(spec),
    # show_svm_config      = plot_svm_classification(spec),
    # show_rf_class_config = plot_rf_classification(spec),
    # show_kmeans_config   = plot_kmeans_clustering(spec),
    # show_dbscan_config   = plot_dbscan_clustering(spec),
    # show_meanshift_config = plot_meanshift_clustering(spec),
    {
      plot.new()
      text(0.5, 0.5, paste("Brak wykresu dla:", spec$method_id))
    }
  )
}

# ========= IMPLEMENTACJA KONKRETNYCH METOD ===========
# ------------ regresja liniowa ------------ 
implement_linear_regression <- function(spec) {
  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )

  #model
  model <- lm(formula, data = spec$data)
  r2 <- summary(model)$r.squared
  r2adj <- summary(model)$adj.r.squared

  y_true <- spec$data[[spec$y_column]]
  y_pred <- predict(model)
  mse <- mean(residuals(model)^2)

  cat("====== OCENA MODELU ====== \n")
  cat("R2 = ")
  cat(round(r2,3))
  cat("\nAdjusted R2 = ")
  cat(round(r2adj,3))
  cat("\nMSE = ")
  cat(round(mse,3))
  cat("\nRMSE = ")
  cat(sqrt(round(mse,3)))
  cat("\n\n")
  cat("====== RÓWNANIE MODELU ====== \n")
  coefs <- coef(model)

formula_text <- paste0(
  "Y = ",
  round(coefs[1],2)
)

for(i in 2:length(coefs)){

  value <- round(coefs[i],3)
  variable <- names(coefs[i])

  sign <- ifelse(value >= 0, " + ", " - ")

  formula_text <- paste0(
    formula_text,
    "\n",
    sign,
    abs(value),
    " * ",
    variable
  )
}
  cat(formula_text)


  return(invisible(model))
}


plot_linear_regression <- function(spec) {
  

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  model <- implement_linear_regression(spec)

  # Dane: observed vs predicted
  preds <- data.frame(
    Observed  = spec$data[[spec$y_column]],
    Predicted = predict(model)
  )

  # Dane: reszty
  residual_data <- data.frame(
    predicted = predict(model),
    residuals = residuals(model)
  )

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # WYKRES 1: Obserwowane vs Przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # WYKRES 2: Diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # WYKRES 3: Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # WYKRES 4: Macierz korelacji
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

  # WYKRES 5: Współczynniki modelu z 95% przedziałami ufności
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

  # WYKRES 6: Histogram reszt z krzywą normalną
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

# -------------- rf ---------------------------
implement_rf_regression <- function(spec) {
  
  set.seed(123)

  formula <- as.formula(
    paste(spec$y_column, "~", paste(spec$x_columns, collapse = " + "))
  )

  #model
  model_rf <- randomForest(formula, data = spec$data)

  # rzeczywiste wartości
  y_true <- spec$data[[spec$y_column]]

  # predykcje
  y_pred <- predict(model_rf, spec$data)

  # liczba obserwacji
  n <- length(y_true)

  # liczba zmiennych niezależnych
  p <- length(spec$x_columns)

  # R²
  r2 <- 1 - sum((y_true - y_pred)^2) /
              sum((y_true - mean(y_true))^2)

  # Adjusted R2
  adj_r2 <- 1 - ((1-r2)*(n-1)/(n-p-1))

  # MSE
  mse <- mean((y_true - y_pred)^2)

  # RMSE
  rmse <- sqrt(mse)

  cat("====== OCENA MODELU ======\n")
  cat("R² =", round(r2,3), "\n")
  cat("Adjusted R² =", round(adj_r2,3), "\n")
  cat("MSE =", round(mse,3), "\n")
  cat("RMSE =", round(rmse,3), "\n")

  cat("\n====== WAŻNOŚĆ ZMIENNYCH ======\n")
  imp <- importance(model_rf)

for(i in rownames(imp)){
    cat(i, ":", round(imp[i,1],3), "\n")
}
  return(invisible(model_rf))
}

plot_rf_regression <- function(spec) {

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  model <- implement_rf_regression(spec)

  observed  <- spec$data[[spec$y_column]]
  predicted <- predict(model, newdata = spec$data)

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

  # WYKRES 1: Obserwowane vs Przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # WYKRES 2: Diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # WYKRES 3: Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # WYKRES 4: Macierz korelacji
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

  # ------------------------------------------------------------------
  # WYKRES 5 (NOWY): Ważność zmiennych (%IncMSE lub IncNodePurity)
  # ------------------------------------------------------------------
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

  # ------------------------------------------------------------------
  # WYKRES 6 (NOWY): Błąd OOB w zależności od liczby drzew
  # ------------------------------------------------------------------
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

# ---------------------- svr --------------------
implement_svr_regression <- function(spec) {

  set.seed(123)

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  data <- spec$data
  model_cols <- c(spec$y_column, spec$x_columns)

  data_model <- data[, model_cols, drop = FALSE]

  # Kolumny numeryczne
  num_cols <- names(data_model)[sapply(data_model, is.numeric)]

  # Parametry skalowania
  scale_params <- list(
    mean = sapply(data_model[, num_cols, drop = FALSE],
                  mean, na.rm = TRUE),
    sd   = sapply(data_model[, num_cols, drop = FALSE],
                  sd,   na.rm = TRUE)
  )

  scale_params$sd[scale_params$sd == 0] <- 1

  # Skalowanie
  data_scaled <- data_model

  data_scaled[, num_cols] <- scale(
    data_model[, num_cols, drop = FALSE],
    center = scale_params$mean,
    scale  = scale_params$sd
  )

  formula <- as.formula(
    paste(
      spec$y_column,
      "~",
      paste(spec$x_columns, collapse = " + ")
    )
  )

  x_num_cols <- intersect(spec$x_columns, num_cols)

  if(length(x_num_cols) > 0){
    gamma_val <- 1 / (
      length(spec$x_columns) *
      mean(
        sapply(
          data_scaled[, x_num_cols, drop = FALSE],
          var
        )
      )
    )
  } else {
    gamma_val <- 1 / length(spec$x_columns)
  }

  model_svr <- svm(
    formula,
    data    = data_scaled,
    type    = "eps-regression",
    kernel  = "radial",
    cost    = 1,
    epsilon = 0.1,
    gamma   = gamma_val
  )

  # Predykcja
  y_true <- spec$data[[spec$y_column]]

  pred_scaled <- predict(
    model_svr,
    newdata = data_scaled
  )

  y_pred <- pred_scaled *
    scale_params$sd[spec$y_column] +
    scale_params$mean[spec$y_column]

  n <- length(y_true)
  p <- length(spec$x_columns)

  mse    <- mean((y_true - y_pred)^2)
  rmse   <- sqrt(mse)
  mae    <- mean(abs(y_true - y_pred))
  r2     <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  adj_r2 <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))

  cat("====== OCENA MODELU ======\n")
  cat("R² =", round(r2, 3), "\n")
  cat("Adjusted R² =", round(adj_r2, 3), "\n")
  cat("MSE =", round(mse, 3), "\n")
  cat("RMSE =", round(rmse, 3), "\n")
  cat("MAE =", round(mae, 3), "\n")

  cat("====== PARAMETRY MODELU ======\n")
  cat("Kernel:", model_svr$kernel, "\n")
  cat("Cost:", model_svr$cost, "\n")
  cat("Gamma:", round(model_svr$gamma, 4), "\n")
  cat("Epsilon:", model_svr$epsilon, "\n")
  cat("\n====== SUPPORT VECTORS ======\n")
  cat("Liczba support vectors:", model_svr$tot.nSV, "\n")

  return(invisible(model_svr))
}

plot_svr_regression <- function(spec) {

  if (is.null(spec$x_columns) || length(spec$x_columns) == 0) {
    return(NULL)
  }

  model <- implement_svr_regression(spec)

  # --- Przygotowanie danych (identyczne skalowanie jak w implement_) ---
  model_cols <- c(spec$y_column, spec$x_columns)
  data_model <- spec$data[, model_cols, drop = FALSE]

  observed <- data_model[[spec$y_column]]

  num_cols <- names(data_model)[sapply(data_model, is.numeric)]

  scale_mean <- sapply(data_model[, num_cols, drop = FALSE], mean, na.rm = TRUE)
  scale_sd   <- sapply(data_model[, num_cols, drop = FALSE], sd,   na.rm = TRUE)
  scale_sd[scale_sd == 0] <- 1

  data_scaled <- data_model
  data_scaled[, num_cols] <- scale(
    data_model[, num_cols, drop = FALSE],
    center = scale_mean,
    scale  = scale_sd
  )

  pred_scaled <- predict(model, newdata = data_scaled)
  predicted   <- as.numeric(
    pred_scaled * scale_sd[spec$y_column] + scale_mean[spec$y_column]
  )

  residual_values <- observed - predicted
  eps             <- model$epsilon *
                       scale_sd[spec$y_column]   # epsilon odskalowane do orig. skali

  preds <- data.frame(
    Observed  = observed,
    Predicted = predicted
  )

  residual_data <- data.frame(
    predicted = predicted,
    residuals = residual_values
  )

  base_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10)
    )

  # WYKRES 1: Obserwowane vs Przewidywane
  p1 <- ggplot(preds, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Obserwowane vs Przewidywane",
      x = "Obserwowane",
      y = "Przewidywane"
    ) +
    base_theme

  # WYKRES 2: Diagram reszt
  p2 <- ggplot(residual_data, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Diagram reszt",
      x = "Wartości przewidywane",
      y = "Reszty"
    ) +
    base_theme

  # WYKRES 3: Q-Q plot reszt
  p3 <- ggplot(residual_data, aes(sample = residuals)) +
    stat_qq(alpha = 0.6) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = "Q-Q plot reszt",
      x = "Teoretyczne kwantyle",
      y = "Próbkowe kwantyle"
    ) +
    base_theme

  # WYKRES 4: Macierz korelacji
  dane_kor         <- data_model
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

  # ------------------------------------------------------------------
  # WYKRES 5 (NOWY): Epsilon-tube – obserwacje vs przewidywane
  # z zaznaczoną strefą epsilon i support vectors
  # ------------------------------------------------------------------
  idx <- seq_along(observed)
  sv_idx <- model$index   # indeksy support vectors w danych skalowanych

  tube_df <- data.frame(
    idx       = idx,
    observed  = observed,
    predicted = predicted,
    is_sv     = idx %in% sv_idx
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

  # ------------------------------------------------------------------
  # WYKRES 6 (NOWY): Rozkład |reszt| vs epsilon – ile obserwacji
  # mieści się w epsilon-tube, ile jest poza
  # ------------------------------------------------------------------
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

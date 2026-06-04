# Podstawowe czyszczenie ramki danych przed dopasowaniem modelu.
# Usuwa puste wiersze/kolumny, ujednolica nazwy, uzupełnia braki medianą (num.)
# lub etykietą "Brak" (char.), zamienia kolumny tekstowe na liczbowe jeśli
# >80% wartości da się sparsować jako liczba.
preprocess_data <- function(data) {
  data <- as.data.frame(data)
  data[data == ""] <- NA

  data <- data[rowSums(is.na(data)) != ncol(data), , drop = FALSE]
  data <- data[, colSums(is.na(data)) != nrow(data), drop = FALSE]

  names(data) <- make.names(names(data), unique = TRUE)
  data <- unique(data)

  # próba konwersji kolumn tekstowych na numeryczne
  data <- data.frame(
    lapply(data, function(col) {
      if (is.character(col)) {
        converted <- suppressWarnings(as.numeric(col))
        ratio <- sum(!is.na(converted)) / length(converted)
        if (ratio > 0.8) return(converted)
      }
      return(col)
    }),
    check.names = FALSE
  )

  # uzupełnianie braków
  data <- data.frame(
    lapply(data, function(col) {
      if (is.numeric(col)) {
        if (!all(is.na(col))) col[is.na(col)] <- median(col, na.rm = TRUE)
      } else {
        col[is.na(col)] <- "Brak"
      }
      return(col)
    }),
    check.names = FALSE
  )

  return(data)
}

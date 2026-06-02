preprocess_data <- function(data) {
  data <- as.data.frame(data)
  data[data == ""] <- NA

  data <- data[rowSums(is.na(data)) != ncol(data), , drop = FALSE]
  data <- data[, colSums(is.na(data)) != nrow(data), drop = FALSE]

  names(data) <- make.names(names(data), unique = TRUE)
  data <- unique(data)

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

  data <- data.frame(
    lapply(data, function(col) {
      if (is.numeric(col)) {
        if (!all(is.na(col))) {
          col[is.na(col)] <- median(col, na.rm = TRUE)
        }
      } else {
        col[is.na(col)] <- "Brak"
      }
      return(col)
    }),
    check.names = FALSE
  )

  return(data)
}

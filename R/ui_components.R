dataset_select <- function(id) {
  selectInput(
    inputId = id,
    label = "Wybierz zbiór danych:",
    choices = c("iris", "Boston", "mtcars")
  )
}
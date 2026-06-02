library(shiny)
library(MASS)

options(shiny.maxRequestSize = 30 * 1024^2)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

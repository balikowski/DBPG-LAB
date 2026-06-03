library(shiny)
library(MASS)
library(ggplot2)
library(patchwork)
library(ggcorrplot)
library(e1071)
library(randomForest)

options(shiny.maxRequestSize = 30 * 1024^2)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

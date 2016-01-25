
wordcor <- function(datafile = "google-nouns-scaled"){
  library(Matrix)
  library(shiny)
  library(DT)
  library(plotrix)
  library(KernSmooth)
  library(Rtsne)
  library(gmra)

  data(datafile)

  ui <- wordcor.ui()
  shinyApp(ui, wordcor.server)
}

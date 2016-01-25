
wordcor <- function(datafile = "google-nouns-scaled"){
  library(Matrix)
  library(shiny)
  library(DT)
  library(plotrix)
  library(KernSmooth)
  library(Rtsne)
  library(gmra)

  assign("datafile", datafile, envir = .GlobalEnv)
  data( list = datafile )

  ui <- wordcor.ui()
  shinyApp(ui, wordcor.server)
}


wordcor <- function(){
  library(Matrix)
  library(shiny)
  library(DT)
  library(plotrix)
  library(KernSmooth)
  library(Rtsne)
  library(gmnra)
  data("google-nouns-scaled")

  ui <- wordcor.ui()
  shinyApp(ui, wordcor.server)
}

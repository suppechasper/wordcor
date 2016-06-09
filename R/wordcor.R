
wordcor <- function(){
  library(Matrix)
  library(shiny)
  library(DT)
  library(plotrix)
  library(KernSmooth)
  library(Rtsne)
  library(gmra)


  choices <-c( "google-1grams: orignal data set",
               "google-1grams-relative: each year scaled by the number of counts",
               "google-1grams-wrelative: each year scaled to sum to one then weighted by the number of different words per year",
               "google-1grams-1800: same as above only including years > 1800",
               "google-1grams-1800-relative: same as above only including years > 1800",
               "google-1grams-1800-wrelative: same as above only including years > 1800"
               )

  x <- select.list(choices)
  datafile <- strsplit(x,":")[[1]][1]
  print(datafile)

  assign("datafile", datafile, envir = .GlobalEnv)
  data( list = datafile )

  ui <- wordcor.ui()
  shinyApp(ui, wordcor.server)
}

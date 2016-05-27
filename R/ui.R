library(shiny)
library(DT)


wordcor.ui <- function(){

fluidPage(

    tabsetPanel(
       tabPanel("Normalized", plotOutput("graph.scaled", brush=brushOpts( "graph1_brush", 
                                                                         direction = "x", resetOnNew=TRUE) ) ),
       tabPanel("Relative Counts", plotOutput("graph.raw", brush=brushOpts( "graph2_brush", 
                                                                         direction = "x", resetOnNew=TRUE) ) ) 
    ),
    fluidRow(
      column(6,
        sliderInput("smoothing", "Smoothing:", min=0, max=10, step = 1, value=5)
      ),
      column(6,
        actionButton("reset", "Reset Selection")
      )
    ),
    hr(),
    fluidRow(
      column(7, 
        #tabsetPanel(
         # tabPanel("Corrleation",
             selectInput("corrplottype", "plot Type", c("1d", "2d", "ortho"), selected = "1d", multiple = FALSE, selectize = TRUE),
             plotOutput( "corr", click="point_click", dblclick="point_dbl_click",
                                           hover="point_hover", brush=brushOpts("point_brush", resetOnNew=TRUE) ),
             fluidRow(
               column(3, 
                 sliderInput("scale.scores", "Scale:", min=0, max=20, step = 1, value=5)
               ),
               column(3, 
                 textInput("primary", "Primary:", value = "", width = NULL, placeholder = NULL)
               )
             ),
             fluidRow(
               column(3, 
                 sliderInput("threshold.scores", "Threshold:", min=0, max=0.2, step = 0.0001, value=0)
               ),
               column(3, 
                 textInput("secondary", "Secondary:", value = "", width = NULL, placeholder = NULL)
               )
             )
          #),
      #    tabPanel("Correlation tsne", plotOutput( "tsne.scores", click="point_click1", dblclick="point_dbl_click1", 
       #                                    hover="point_hover1", brush=brushOpts("point_brush1", resetOnNew=TRUE) ) ),
        #  tabPanel("Counts tsne", plotOutput( "tsne", click="point_click2", dblclick="point_dbl_click2", 
         #                                  hover="point_hover2", brush=brushOpts("point_brush2", resetOnNew=TRUE) ),
          #              sliderInput("scale.counts", "Scale:", min=0, max=20, step = 1, value=5)
           #        ) 
        #)
      ),
      column(5,
        DT::dataTableOutput('table')
      )
    )
)

}


wordcor.ui()


library(shiny)
library(DT)


wordcor.ui <- function(){

fluidPage(

    tabsetPanel(
       tabPanel("Normalized Score", plotOutput("graph.scaled", dblclick="graph1_click",
                                         brush=brushOpts( "graph1_brush",
                                                         direction = "x",
                                                         resetOnNew=TRUE) ) ),
       tabPanel("Raw Data", plotOutput("graph.raw", brush=brushOpts( "graph2_brush", 
                                                                         direction = "x", resetOnNew=TRUE) ) ),
       tabPanel("Wavelet powers", plotOutput("wav.pow", brush=brushOpts( "wav_brush", 
                                                                         resetOnNew=TRUE) ) )  
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
      column(3, 
        textInput("primary", "Primary:", value = "", width = NULL, placeholder = NULL)
      ),
      column(3, 
        selectInput("primary.shape", "Shape:", choices = c("none", "linc", "ldec", "sine" ) )
      ),
      column(6,
         plotOutput( "corrprimary", click="point_click", dblclick="point_dbl_click",
                                           hover="point_hover", brush=brushOpts("primary_brush", resetOnNew=TRUE), width="100%", height=50 )
      )
    ),
    fluidRow(
      column(3, 
        textInput("secondary", "Secondary:", value = "", width = NULL, placeholder = NULL)
      ),
      column(3, 
        selectInput("secondary.shape", "Shape:", choices = c("none", "linc", "ldec", "sine" ) )
      ),
      column(6, 
         plotOutput( "corrsecondary", click="point_click", dblclick="point_dbl_click",
                                           hover="point_hover", brush=brushOpts("secondary_brush", resetOnNew=TRUE), width="100%", height=50 )
      )
    ),
    fluidRow(
      column(8, 
        #tabsetPanel(
         # tabPanel("Corrleation",
             plotOutput( "corr", click="point_click", dblclick="point_dbl_click",
                                           hover="point_hover",  height=400, width=400 ),
             fluidRow(
               column(2, 
                 sliderInput("scale.scores", "Scale:", min=0, max=20, step = 1, value=5)
               ),
               column(2, 
                 sliderInput("threshold.scores", "Threshold:", min=0, max=0.4, step = 0.0001, value=0)
               ),
               column(2, 
                 selectInput("corrplottype", "Plot Type", c("oblique", "ortho"), selected = "1d", multiple = FALSE, selectize = TRUE)
               ),
               column(2, 
                 checkboxInput("absolute", "Absolute Value")
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
      column(4,
        tabsetPanel(
          tabPanel( "Corrlations", DT::dataTableOutput('table') ),
          tabPanel( "Derivs Raw", DT::dataTableOutput('sderivtable') ),
          tabPanel( "Derivs Score", DT::dataTableOutput('derivtable') ),
          tabPanel( "Wavlet", DT::dataTableOutput('wavtable') )
        )
      )
    )
)

}


wordcor.ui()


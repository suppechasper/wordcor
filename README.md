# Word Correlation Explorer
Shiny app for word correlation exploration.

## Installation ##

1. Install R
2. Start R
3. In R run the commands:
    1. install.packages( c("shiny, "DT", "Matrix", "KernSmooth", "plotrix", "Rtsne", "devtools") )
    2. devtools::install_github("suppechasper/gmra")
    3. devtools::install_github("suppechasper/wordcor")
     
    
## Running ##

1. Start R.
2. In R run the commands:
   1. library(WordCor)
   2. wordcor()

Note wordcor will save smoothed version of the data in the current diretory you run wordcor(). If your it in the same directory again it will reuse the smoothed versions and not recompute them.

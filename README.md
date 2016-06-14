# Word Correlation Explorer
Shiny application for word correlation exploration.

## Installation ##

1. Install R
2. Start R
3. In R run the commands:
    1. install.packages( c("shiny", "DT", "Matrix", "KernSmooth", "plotrix", "Rtsne", "devtools") )
    2. library("devtools")
    3. devtools::install_github( c("suppechasper/gmra", "suppechasper/wordcor") )
     
    
## Running ##

1. Start R.
2. In R run the commands:
   1. library(WordCor)
   2. wordcor()

Note wordcor will save smoothed version of the data in the current directory
you run wordcor(). If your it in the same directory again it will reuse the
smoothed versions and not recompute them.

## Usage ##
The application will prompt you which data set to use. Currently there is six
options of variations from the Google 1grams data set. Either the complete data
set or restricted to the years after 1800. For both there are three options,
original data set, relative scaling (each years relative frequency) and
weighted relative frequency with additional scaling by the number of different
words used per year.

The application user interface consists of multiple linked displays. 

1. The top graphs shows the primary (red) and secondary (orange) selected words as well as selections made in the plots below. The dots are the positive (light gray) and negative (dark gray) derivative summed over all words. 
  1. Brushing years will restrict correlation computations to the selected years. Reset years will select all years.
  2. Double clicking will select the top 20 words with the most positive and most negative derivatives in that year.

2. The wavelet plot shows the sum of the wvaelet coefficients for all word (weighted by phase).
  1. Selecting a region will select 40 words with the largest wavelet coefficients for that year and period length.

3. The primary and secondary selected word and the correlation to all other words represented on a line from -1 to +1 correlation. 
  1. The primary and secondary word can be changed through text input or clicking (for primary) or double clicking (for secondary) on a word represented on th line representation.

4. A correlation plot of with respect to primary and secondary.  
   1. The primary and secondary word can be changed through clicking (for
      primary) or double clicking (for secondary) on points in the scatter
plot.
   2. Brushing selects points to be shown in the table on the right and in the
      graphs above.
   3. Scale selects the level of clustering, i.e. if individual words should be
      shown in the plot (large scale) or words with similar correlation be
clustered together (small scale).
   4. Plot type selects the type of plot. Oblique is putting the primary and
      secondary correlation lines orthogonal to each other and plotting each
point at the corresponding correlation values to the primary and secondary.
Ortho better preserves pairwise relationships in the plot (for details see
http://mckennapsean.com/scorrplot/ ).
   5. Threshold removes words with low counts.
   6. Absolute Value selects whether positive and negative correlation should be distinguished or not.

5. Table shows selected words in the correlation plot, largest derivatives for
   the selected year, and largest wavelet cofficients for a selected region in
the wavelet image.
   1. Clicking on rows will highlight the words in the graphs. Selection in the
      correlation table appear blue, derivative of the score table dark green,
derivative of the raw data in light green, and wavelet coefficient table in
dark pink. 

  

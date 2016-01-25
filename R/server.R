###  Word correlation server ###

## Data Pipeline ##
#
# Raw data is smoothed, then sliced and transformed into z-scores and used as input to gmra
#
#            -> smoothed.scaled -> sliced.scores -> gmra.scores
# data.raw -|
#            -> smoothed.counts -> sliced.counts -> gmra.counts
#
# The smoothing is sampled at a fixed interval such that differently smoothed
# z-Scores can be compared in teh correlation plot. Using a sampling scheme that
# depends on the amount of smoothing can reduce data size and computation speeds.
# However the primary and seocndary would need to change when the smoothing level 
# is changed and results in visual discontinouity.
#
# It is setup such that any change in the pipeline will cause an update of 
# the remaining pipleine through shiny's reactive framework.

## Display ##
#
# Plots use gmra clusterd points forcorrelation plot projection 
# using primary and secondary, t-stcoahctsic neighbrohood embeddings 
# on scores and counts.




wordcor.server <- function(input,output,session){

  datafile = "google-nouns-scaled"

  ## Reactive variables ##
  selection <- reactiveValues()
  selection$brushed <- c()
  selection$years <- list( start = c(), end = c() )
    
  #reset selection when gmra scale is changed
  scale.score.reset <- observeEvent( input$scale.scores, {
    selection$brushed <<- c()
  })    


  
  
  
  #smoothed data depends on raw data
  smoothed <- reactive({
    if(input$smoothing > 0 ){
        fname <- sprintf("%s-smoothed-%.2f.Rdata", datafile, input$smoothing)
        if( file.exists( fname ) ){
          withProgress(message = 'Loading smoothed data', value = 0, {
            load(fname)
            res
          })
        }
        else{
          withProgress(message = 'Smoothing', value = 0, {
            years <- c()
            n <- ncol(data$raw) #round( min( ncol(data$raw), ncol(data$raw) / input$smoothing)  )
            S <- matrix(nrow=nrow(data$raw), ncol=n) 
            R <- matrix(nrow=nrow(data$raw), ncol=n) 

            for(i in 1:nrow(data$raw) ){
              incProgress( 1 / nrow(data$raw), detail = data$words[i] )
              lp <-  locpoly( x = minYear:maxYear, data$raw[i, ], bandwidth = input$smoothing, 
                            gridsize=n, range.x=c(minYear, maxYear) )
              R[i, ] <- lp$y 
              S[i, ] <- lp$y / max(lp$y)
              years <- lp$x
            }
            rownames(S) = data$words
            rownames(R) = data$words
            res <- list(years = years, counts = R, scaled = S)
            save(res, file=fname)
            res
          })
        }
      }
      else{
        list(years = minYear:maxYear, counts = data$raw, scaled=data$scaled)
      }
  })

  #sliced data depends on smoothed data
  complete <- c() 
  sliced <- reactive({
     X <- smoothed()$counts
     years = smoothed()$years
     if( length( selection$years$start ) > 0 ){
       index <- c()
       for(i in 1:length(selection$years$start) ){
         index <- c(index, which( smoothed()$years < selection$years$end[i] & 
                                  smoothed()$years > selection$years$start[i] ) )
       }
       X <- X[,index]
       years = smoothed()$years[ index ]
     }
     Xs <- t( scale( t( X ) , scale=T, center=T) ) 
     Xs <- Xs / sqrt(ncol(Xs)-1)
     rownames(Xs) <- data$words
     rownames(X) <- data$words
     index = which( complete.cases(Xs) )
     Xs <- Xs[index, ]
     X <- X[index, ]
     complete <<- index
     list( scores = Xs, counts= X, years = years )
  })


  ### Multiscale decompositions depends on sliced data
  gmra <- list()

  gmra$scores <- reactive({
    withProgress(message = "Clustering scores", value=0, {
       gmra.create.ikm( sliced()$scores, eps = 0.000001, nKids = 4, similarity = 2)
    })
  })
  
  gmra.scores <- reactive({
    isolate( selection$brushed <<- c() )
    gmra.centers(gmra$scores(), input$scale.scores)
  })

  gmra.partition.scores <- reactive({
    gmra.partition( gmra$scores(), input$scale.scores)
  })
  
  gmra.index.scores <- function(ind){
    index <- c()
    if(length(ind) > 0 ){
      for(i in 1:length(ind) ){
        index <- c( index, gmra.partition.scores()[[ind[i]]] )
      }
    }
    index
  }

  raw.index.scores <- function(ind){
    index <- gmra.index.scores(ind)
    complete[index]
  }


  


  gmra$counts <- reactive({
    withProgress(message =  "Clustering counts", value=0, {
      gmra.create.ikm(sliced()$counts, eps = 0.000001, nKids = 4, similarity = 1)
    })
  })

  gmra.counts <- reactive({
    gmra.centers(gmra$counts(), input$scale.counts)
  })

  gmra.partition.counts <- reactive({
    gmra.partition( gmra$counts(), input$scale.scores)
  })
  
  raw.index.counts <- function(ind){
    index <- c()
    if(length(ind) > 0 ){
      for(i in 1:length(ind) ){
        index <- c(index, gmra.partition.counts()[[ind]] )
      }
    }
    index
  }



  ### T-sne embeddings depens on gmra data

  tsne.scores <- reactive({
    withProgress(message = "Computing t-sne", value = 0, {
      x <- Rtsne( gmra.scores(), check_duplicates=F, pca=F)
      data.frame( x = x$Y[,1], y = x$Y[, 2] )
    })
  })
 

  tsne <- reactive({
    withProgress(message = "Computing t-sne", value = 0, {
      x <- Rtsne( gmra.counts(), check_duplicates=F, pca=F)
      data.frame( x = x$Y[,1], y = x$Y[, 2])
    })
  })


    

  ### Projected data ###
  # depends on gmra data and primary and secondary
  # primary and seocndary go through the same pipeline as data.raw 
  #

  #Projection definiton points  
  projector <- reactiveValues()
  projector$primary <- data$raw[1, , drop=FALSE]
  projector$secondary <- data$raw[2, , drop=FALSE]

  #update primary and seocndary text inputs
  primaryObserve <- observe({
    w <- row.names(projector$primary)
    updateTextInput(session, "primary", value = w)
  })

  secondaryObserve <- observe({
    w <- row.names(projector$secondary)
    updateTextInput(session, "secondary", value = w)
  })

  #change primary and esconadry based on text input
  primaryInput <- observeEvent(input$primary, {
    index <- which( data$words == input$primary )
    if(length(index) == 1){    
      x <- data$raw[index, , drop=FALSE]
      if( !all(x == projector$primary)  ){
        projector$primary <<- x
      }
    }
  })

  secondaryInput <- observeEvent(input$secondary, {
    index <- which( data$words == input$secondary )
    if(length(index) == 1){    
      x <- data$raw[index, , drop=FALSE]
      if( !all(x == projector$secondary)  ){
         projector$secondary <<- x
      }
    }
  })

  #primary pipeline
  primary.smoothed <- reactive({
    if(input$smoothing > 0 ){
      n <- ncol(data$raw)
      lp <-  locpoly( x = minYear:maxYear, y=projector$primary, bandwidth = input$smoothing, 
                            gridsize=n, range.x=c(minYear, maxYear) )
      lp$y  
    }
    else{
      projector$primary    
    } 
  })
  
  primary.sliced <- reactive({
     x <- primary.smoothed()
     years = smoothed()$years
     if( length( selection$years$start ) > 0 ){
       index <- c()
       for(i in 1:length(selection$years$start) ){
         index <- c(index, which( smoothed()$years < selection$years$end[i] & 
                                  smoothed()$years > selection$years$start[i] ) )
       }
       x <- x[index]
       years = smoothed()$years[ index ]
     }
     xs <- x - mean(x)
     l <- sqrt(sum(xs^2)) 
     if( l == 0 ){
       x <- sliced()$counts[1, ]
     }
     xs <- x - mean(x)
     l <- sqrt(sum(xs^2)) 
     xs = xs / l
     list(score=xs, counts = x, years=years)
  })

  primary.projected <- reactive({
     c(1, 0) 
  })

  #secondary pipeline
  secondary.smoothed <- reactive({
    if(input$smoothing > 0 ){
       n <- ncol(data$raw)
       lp <-  locpoly( x = minYear:maxYear, y = projector$secondary, bandwidth = input$smoothing, 
                            gridsize=n, range.x=c(minYear, maxYear) )
       lp$y  
    }
    else{
      projector$secondary   
    } 
  })
  
  secondary.sliced <- reactive({
     x <- secondary.smoothed()
     years = smoothed()$years
     if( length( selection$years$start ) > 0 ){
       index <- c()
       for(i in 1:length(selection$years$start) ){
         index <- c(index, which( smoothed()$years < selection$years$end[i] & 
                                  smoothed()$years > selection$years$start[i] ) )
       }
       x <- x[index]
       years = smoothed()$years[ index ]
     }
     xs <- x - mean(x)
     l <- sqrt(sum(xs^2)) 
     if( l == 0 ){
       x <- sliced()$counts[2, ]
     }
     xs <- x - mean(x)
     l <- sqrt(sum(xs^2)) 
     xs = xs / l
     list(score=xs, counts = x, years=years)
  })

  ortho <- reactive({
    a <-  primary.sliced()$score %*% secondary.sliced()$score
    o <- secondary.sliced()$score - a * primary.sliced()$score
    o / sqrt( sum( o^2 ) )
  })

  secondary.projected <- reactive({
     c( primary.sliced()$score %*% secondary.sliced()$score, 
        ortho() %*% secondary.sliced()$score )
  })


  #Projected data
  projected <- reactive( {
    X <- gmra.scores()
    Z = data.frame( x = X %*% primary.sliced()$score, 
                    y = X %*% ortho() )
    Z
  } ) 




  ### Observers ###

  ## Years selection ##
  selectYears <- function(ev){
    if( !is.null( ev ) ){
       ystart <- selection$years$start
       yend <- selection$years$end
       ystart <- c(ystart, ceiling( ev$xmin ) )
       yend <- c(yend, ceiling( ev$xmax ) )
       
       index <- c()
       for(i in 1:length(ystart) ){
         index <- c(index, ystart[i]:yend[i])
       }
       index <- sort( unique(index) )

       ystart <- c( index[1] )
       yend <- c()
       for( i in 2:length(index) ){
         if(index[i]-1 != index[i-1]){
           yend <- c(yend, index[i-1] )
           ystart <- c(ystart, index[i] )
         }
       }
       yend <- c(yend, index[ length(index) ] )
       
       selection$years <<- list(start=ystart, end=yend)
     }
  }

  obYears1 <- observeEvent(input$graph1_brush, {
     selectYears(input$graph1_brush)     
  })

  obYears2 <- observeEvent(input$graph2_brush, {
     selectYears(input$graph2_brush)     
  })

  btnYears <- observeEvent(input$reset, {
    selection$years <<- list(start=c(), end=c() )
  })


  #mouse cliks in correlation and tsne plots for changing primary and secondary
  ob1a <- observeEvent(input$point_click, {
    if( !is.null( input$point_click) ){
      ev <- input$point_click
      tmp <- myNearPoints( projected(), ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
      if( length(tmp) == 1){
         sel <- raw.index.scores(tmp)
         if(length(sel) == 1){
           projector$primary <<- data$raw[sel, , drop=FALSE]
         }
         else{
           updateTextInput(session, "primary", value="averaged word")
           projector$primary <<- apply( data$raw[sel, ], 2 , mean )
         }

      }
    }
  })

  ob1b <- observeEvent(input$point_click1, {
    if( !is.null( input$point_click1) ){
      ev = input$point_click1
      tmp <- myNearPoints( tsne.scores(), ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
      if( length(tmp) == 1){
         sel <- raw.index.scores(tmp)
         if(length(sel) == 1){
           projector$primary <<- data$raw[sel, , drop=FALSE]
         }
         else{
           updateTextInput(session, "primary", value="averaged word")
           projector$primary <<- apply( data$raw[sel, ], 2 , mean )
         }

      }
    }
  })

#  ob1c <- observe({
#    if( !is.null( input$point_click2) ){
#      ev = input$point_click2
#      tmp <- myNearPoints( tsne(), ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
#      if( length(tmp) == 1){
#        #selection$primary <<- tmp
#      }
#    }
#  })

  ob2a <- observeEvent(input$point_dbl_click, {
     ev = NULL
    if( !is.null( input$point_dbl_click) ){
      ev = input$point_dbl_click
      tmp <- myNearPoints( projected(), ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
      if( length(tmp) == 1){
        sel <- raw.index.scores(tmp)
         if(length(sel) == 1){
           projector$secondary <<- data$raw[sel, , drop=FALSE]
         }
         else{
           updateTextInput(session, "secondary", value="averaged word")
           projector$secondary <<- apply( data$raw[sel, ], 2 , mean )
         }
      }

    }
  })

  ob2b <- observeEvent(input$point_dbl_click1, {
    if( !is.null( input$point_dbl_click1) ){
      ev = input$point_dbl_click1
      tmp <- myNearPoints(  tsne.scores(), ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
      if( length(tmp) == 1){
         sel <- raw.index.scores(tmp)
         if(length(sel) == 1){
           projector$secondary <<- data$raw[sel, , drop=FALSE]
         }
         else{
           updateTextInput(session, "secondary", value="averaged word")
           projector$secondary <<- apply( data$raw[sel, ], 2 , mean )
         }
      }

    }
  })

#  ob2c <- observe({
#    if( !is.null( input$point_dbl_click2) ){
#      ev = input$point_dbl_click2
#      tmp <- myNearPoints( embeddings$tsne, ev, xvar="x", yvar="y", threshold = 10, maxpoints = 1)
#      if( length(tmp) == 1){
#        selection$secondary <<- tmp
#      }
#    }
#  })


  #point brushing
  ob3a <- observeEvent(input$point_brush, {
    if( !is.null(input$point_brush) ){
      ev = input$point_brush
      tmp <- myBrushedPoints( isolate( projected() ), ev, xvar="x", yvar="y")
      if(length(tmp) > 0 ){
        selection$brushed <<- tmp
      }

    }
  })

  ob3b <- observeEvent(input$point_brush1, {
    if( !is.null( input$point_brush1) ){
      ev = input$point_brush1
      tmp <- myBrushedPoints( tsne.scores(), ev, xvar="x", yvar="y")
      if(length(tmp) > 0 ){
        selection$brushed <<- tmp
      }
    }
  })

#  ob3c <- observe({
#    if( !is.null( input$point_brush2) ){
#      ev = input$point_brush2
#      tmp <- myBrushedPoints( tsne(), ev, xvar="x", yvar="y")
#      if(length(tmp) > 0 ){
#        selection$brushed <<- tmp
#      }
#    }
#  })



  ## Correaltion plot ##
  output$corr <- renderPlot({
    X = projected()
    
    cols = rep("#00000010", nrow(X) )
    cols[ selection$brushed ] = "#66339950"
    plot( y ~ x, data=X, type="p", xlim = c( -1.1, 1.1 ), ylim = c( -1.1, 1.1 ), asp=1, 
          pch=19, col=cols, bty="n", axes=FALSE, xlab="", ylab="" )
    
    if( length(input$table_rows_selected) > 0 ){
      Sp <- cbind( sliced()$scores[ input$table_rows_selected, ] %*% primary.sliced()$score,
             sliced()$scores[ input$table_rows_selected, ] %*% ortho() )
      points(Sp, pch=19, col="#0000FF")
    }

    
    points( rbind(primary.projected(), secondary.projected()  ), pch=19, col=c("red", "orange") )

    draw.circle(0,0, 1, nv=100, border="gray",col=NA, lty=1, lwd=1)
    lines(c(-1,1), c(0,0), col="black", lwd = 2)
    lines(c(0,0), c(-1,1), col="black", lwd = 2)

  }, width=500, height=500 )

  
  ## t-sne plots ##

  output$tsne.scores <- renderPlot({
    X <-  tsne.scores()
    cols = rep("#00000010", nrow(X) )
    cols[ selection$brushed ] = "#66339950"
    plot( y ~ x, X, type="p",asp=1, pch=19, col=cols, bty="n", axes=FALSE,
         xlab="", ylab="" )
  }, width=500, height=500 )



  output$tsne <- renderPlot({
    X <- tsne()
    cols = rep("#00000010", nrow(X) )
    cols[ selection$brushed ] = "#FF00FF50"
    plot( y ~ x, X, type="p", asp=1, pch=19, col=cols, bty="n", axes=FALSE,
         xlab="", ylab="" )

  }, width=500, height=500 )



  #Tbale output for slected points
  output$table <- renderDataTable({
    sel <- gmra.index.scores( selection$brushed )
    corp <- sliced()$scores[sel, ]  %*% primary.sliced()$score
    cors <- sliced()$scores[sel, ]  %*% secondary.sliced()$score
    words <- rownames( sliced()$scores )
    A <- cbind(words[ sel ], round(corp, 2), round(cors,2) )
    colnames(A) <- c("word", "cor( p )", "cor( s )" )
    DT::datatable(A)
  } )



  #Scaled timeline smoothed
  output$graph.scaled <- renderPlot({
    sel <- raw.index.scores( selection$brushed )
    
    X <- smoothed()$scaled 
    years <- smoothed()$years

    yMax = 1
    if(length(sel) > 0 ){
      #hack
      if(length(sel) == 1){
        sel = rep(sel, 2)
      }
      meanC = apply( X[sel, ], 2, mean )
      sdC = apply( X[sel, ], 2, sd )
      maxC = max(meanC+sdC)
    }
   
    p <- primary.smoothed() 
    p <- p - min(p)
    p <- p /max(p) 
    plot( x=years, p, ylim=c(0, yMax), 
          xlim=c(minYear, maxYear), col="red", type="l", 
          bty="n", xlab="years", ylab="counts", lwd=3 )
    
    s <- secondary.smoothed()
    s <- s - min(s)
    s <- s /max(s) 
    lines(x=years, s, col="orange", lwd=3)

    if(length(sel) > 0 ){
      lines(x=years, meanC/maxC, col="#00000060", lwd=2)
      lines(x=years, (meanC + sdC) /maxC, col="#00000060", lwd=2)
      tmp <- meanC - sdC
      tmp[tmp<0] = 0
      lines(x=years, tmp/maxC, col="#00000060", lwd=2)
    }
    
    sel  = input$table_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="blue", lwd=2)
      }
    } 

    if( length(selection$years$start) > 0 ){
      for(i in 1:length(selection$years$start) ){
        abline( v = selection$years$start[i] )
        abline( v = selection$years$end[i] )
        lines( c( selection$years$start[i], selection$years$end[i] ), c(1, 1) )
      }
    }

  })


  #Counts timeline smoothed
  output$graph.raw <- renderPlot({
    sel <- raw.index.scores( selection$brushed )
    X <- smoothed()$counts 
    years <- smoothed()$years
   
    maxC <- 0
    maxC <- max( c( primary.smoothed(), secondary.smoothed() ) )
    if(length(sel) > 0 ){
      meanC = apply( X[sel, ], 2, mean )
      sdC = apply( X[sel, ], 2, sd )
      maxC = max(c(maxC, meanC+sdC) )
    }

    plot( NA, ylim=c(0, maxC), 
          xlim=c(minYear, maxYear), col="red", type="l", 
          bty="n", xlab="years", ylab="counts", lwd=3 )
      lines(x=years, primary.smoothed(), col="red", lwd=3)
      lines(x=years, secondary.smoothed(), col="orange", lwd=3)


    if( length(sel) > 0 ){
      lines(x=years, meanC, col="#00000060", lwd=2)
      lines(x=years, (meanC + sdC), col="#00000060", lwd=2)
      tmp <- meanC - sdC
      tmp[tmp<0] = 0
      lines(x=years, tmp, col="#00000060", lwd=2)
    }

    sel  = input$table_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="blue", lwd=2)
      }
    }

    if( length(selection$years$start) > 0 ){
      for(i in 1:length(selection$years$start) ){
        abline( v = selection$years$start[i] )
        abline( v = selection$years$end[i] )
        lines( c( selection$years$start[i], selection$years$end[i] ), c(maxC, maxC) )
      }
    }

  } )

}




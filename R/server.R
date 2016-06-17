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


  ## Reactive variables ##
  selection <- reactiveValues()
  selection$brushed.secondary <- c()
  selection$brushed.primary <- c()
  selection$derivs <- NULL
  selection$derivYear <- NULL
  selection$years <- list( start = c(), end = c() )
  selection$wavs <- NULL
 

    
  #reset selection when gmra scale is changed
  scale.score.reset <- observeEvent( input$scale.scores, {
    selection$brushed.primary <<- c()
    selection$brushed.secondary <<- c()
  })    

  
  
  
  
  #smoothed data depends on raw data
  smoothed <- reactive({
      if(input$smoothing > 0 ){
        fname <- sprintf("%s-smoothed-%.2f.rda", datafile, input$smoothing)
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

  smoothed.derivative <- reactive({
      if(input$smoothing > 0 ){
        fname <- sprintf("%s-smoothed-%.2f-derivative.rda", datafile, input$smoothing)
        if( file.exists( fname ) ){
          withProgress(message = 'Loading smoothed derivative data', value = 0, {
            load(fname)
            res
          })
        }
        else{
          withProgress(message = 'Smoothing derivative', value = 0, {
            years <- c()
            n <- ncol(data$raw) #round( min( ncol(data$raw), ncol(data$raw) / input$smoothing)  )
            derivs <- matrix(nrow=nrow(data$raw), ncol=n) 

            positive <- 0
            negative <- 0
            for(i in 1:nrow(data$raw) ){
              incProgress( 1 / nrow(data$raw), detail = data$words[i] )
              lp <-  locpoly( x = minYear:maxYear, data$raw[i, ], bandwidth = input$smoothing, 
                            gridsize=n, range.x=c(minYear, maxYear), drv=1 )
             
              years <- lp$x 
              derivs[i, ] = lp$y
              
              
            }
            derivs.n <- t( t(derivs) / sqrt( rowSums(smoothed()$counts^2) ) )
           
            positive <- derivs
            positive[positive<0] = 0
            positive = colSums(positive)
            negative <- derivs
            negative[negative>0] = 0
            negative = colSums(negative)

            positive.n <- derivs.n
            positive.n[positive.n<0] = 0
            positive.n = colSums(positive.n)
            negative.n <- derivs.n
            negative.n[negative.n>0] = 0
            negative.n = colSums(negative.n)


            res <- list(years = years, positive=positive, negative=negative, derivs=derivs, 
                        derivs.scaled = derivs.n, positive.scaled=positive.n, negative.scaled=negative.n)
            save(res, file=fname)
            res
          })
        }


      }
      else{
        list(years = minYear:maxYear)
      }
      
  })

  #sliced data depends on smoothed data
  complete <- c() 
  sliced <- reactive({
     isolate( selection$brushed.primary <<- c() )
     isolate( selection$brushed.secondary <<- c() )
     X <- smoothed()$counts
     sumX <- rowSums(X) 
     
     #maxThreshold = round( max( sumX ), 2)
     #updateSliderInput(session, "threshold.scores", max=maxThreshold, step=maxThreshold/100)
     
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
     index = which( complete.cases(Xs)  &  sumX > input$threshold.scores )

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
    isolate( selection$brushed.primary <<- c() )
    isolate( selection$brushed.secondary <<- c() )
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
  # primary and secondary go through the same pipeline as data.raw 
  #

  #Projection definiton points  
  projector <- reactiveValues()
  projector$primary <- data$raw[1, , drop=FALSE]
  projector$secondary <- data$raw[2, , drop=FALSE]

  #update primary and seocndary text inputs
  primaryObserve <- observe({
    w <- row.names(projector$primary)
    isolate( selection$brushed.primary <<- c() )
    updateTextInput(session, "primary", value = w)
  })

  secondaryObserve <- observe({
    w <- row.names(projector$secondary)
    isolate( selection$brushed.secondary <<- c() )
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

  #change primary and esconadry based on select input
  primarySelect <- observe({
     type <- input$primary.shape
     x <- rep(0, ncol(data$raw) )
     if( length(selection$years$start) == 0){
       ys <- minYear
       ye <- maxYear
       ind <- 1:length(x)
     }
     else{
        ys <- selection$years$start[1]
        ye <- selection$years$end[1]
        y <- minYear:maxYear 
       ind <- which( y > ys & y<ye)
     }
     if(type == "linc"){
       x[ind] <- 1:length(ind)
     }
     else if(type == "ldec"){
       x[ind] <- length(ind):1
     
     }
     else if(type == "sine"){
        a <- 1:length(ind)
        x[ind] <- sin( (a-1)/(length(ind)-1) * pi )
     
     }
     if( type != "none" ){
       x <- x - mean(x)
       x <- x / sqrt( sum(x^2) )
       x <- t(x)
       row.names(x) <- paste0("shape.",type)
       isolate(projector$primary <<- x)
     }
  })

  secondarySelect <- observe({
     type <- input$secondary.shape
     x <- rep(0, ncol(data$raw) )
     if( length(selection$years$start) == 0){
       ys <- minYear
       ye <- maxYear
       ind <- 1:length(x)
     }
     else{
        ys <- selection$years$start[1]
        ye <- selection$years$end[1]
        y <- minYear:maxYear 
       ind <- which( y > ys & y<ye)
     }
     if(type == "linc"){
       x[ind] <- 1:length(ind)
     }
     else if(type == "ldec"){
       x[ind] <- length(ind):1
     
     }
     else if(type == "sine"){
        a <- 1:length(ind)
        x[ind] <- sin( (a-1)/(length(ind)-1) * pi )
     
     }
     if( type != "none" ){
       x <- x - mean(x)
       x <- x / sqrt( sum(x^2) )
       x <- t(x)
       row.names(x) <- paste0("shape.",type)
       isolate(projector$secondary <<- x)
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
     c( 1, ortho() %*% primary.sliced()$score ) 
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
    o <- rep(0, length( primary.sliced()$score ) ) 
    if(input$corrplottype=="ortho"){
      a <- primary.sliced()$score %*% secondary.sliced()$score
      o <- secondary.sliced()$score - a * primary.sliced()$score
      o <- o / sqrt( sum( o^2 ) )
    }
    else if(input$corrplottype=="oblique"){
      o <- secondary.sliced()$score 
    }
    o
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
    if(input$absolute){
      Z = abs(Z)
    }
    Z
  } ) 

  projected.primary <- reactive( {
    X  = gmra.scores() %*% primary.sliced()$score
    if(input$absolute){
      X = abs(X)
    }
    data.frame(x=X, y = rep(0, length(X) ) )
  } ) 
  
  projected.secondary<- reactive( {
    X = gmra.scores() %*% secondary.sliced()$score
    if(input$absolute){
      X = abs(X)
    }
    data.frame(x=X, y = rep(0, length(X) ) )
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


  ob.graph1.click <- observeEvent(input$graph1_click, {
    if( !is.null(input$graph1_click) ){
      xy <- which.min( abs(smoothed.derivative()$years-input$graph1_click$x) )
      selection$derivYear <<- input$graph1_click$x

      vals <- smoothed.derivative()$derivs[ ,xy]
      dor <- order( vals )
      index <- dor[c(1:20, length(dor):(length(dor)-20) )]
      selection$derivs <<- list(index =index, vals=vals[index])
      
      vals <- smoothed.derivative()$derivs.scaled[ ,xy]
      dor <- order( vals )
      index <- dor[c(1:20, length(dor):(length(dor)-20) )]
      selection$derivs.scaled <<- list(index=index, vals=vals[index])
    }
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

  ob.primary.brush <- observeEvent(input$primary_brush, {
    if( !is.null(input$primary_brush) ){
      ev = input$primary_brush
      tmp <- myBrushedPoints( isolate( projected.primary()  ), ev, xvar="x", yvar="y")
      if(length(tmp) > 0 ){
        selection$brushed.primary <<- tmp
      }

    }
  })

  ob.secondary.brush <- observeEvent(input$secondary_brush, {
    if( !is.null(input$secondary_brush) ){
      ev = input$secondary_brush
      tmp <- myBrushedPoints( isolate( projected.secondary() ), ev, xvar="x", yvar="y")
      if(length(tmp) > 0 ){
        selection$brushed.secondary <<- tmp
      }

    }
  })






  ## Correaltion plot ##
  output$corr <- renderPlot({
    X = projected()

    
    cols = rep("#00000010", nrow(X) )
    cols[ selection$brushed.primary ] = "#0000FF50"
    cols[ selection$brushed.secondary ] = "#0000FF50"

    pointtype = 19
    par(mar=c(0.5, 0.5, 0.5, 0.5) )
    plot( y ~ x, data=X, type="p", xlim = c( -1.1, 1.1 ), ylim = c( -1.1, 1.1 ), asp=1, 
          pch=pointtype, col=cols, bty="n", axes=FALSE, xlab="", ylab="" )
   
    #if( length(input$table_rows_selected) > 0 ){
    #  Sp <- cbind( sliced()$scores[ input$table_rows_selected, ] %*% primary.sliced()$score,
    #         sliced()$scores[ input$table_rows_selected, ] %*% ortho() )
    #  if(input$absolute){
    #    Sp = abs(Sp)
    #  }
#
#      points(Sp, pch=19, col="#0000FF")
#    }

    if(input$absolute){
      points( abs( rbind( primary.projected(), secondary.projected()  ) ), pch=19, col=c("red", "orange") )
    }
    else{
      points( rbind( primary.projected(), secondary.projected()  ), pch=19, col=c("red", "orange") )
    }

    if(input$corrplottype == "ortho" ){
      draw.circle(0,0, 1, nv=100, border="gray",col=NA, lty=1, lwd=2)
      lines(c(-1,1), c(0,0), col="black", lwd = 3)
      lines( c(secondary.projected()[1],-secondary.projected()[1]), 
             c(secondary.projected()[2],-secondary.projected()[2]), 
             col="black", lwd = 3)
      
      #lines(c(-0.5,0.5), c(-0.5,-0.5), col="black", lwd = 1)
      #lines(c(-0.5,0.5), c(0.5,0.5), col="black", lwd = 1)
      lines(c(0.5,0.5), c(-0.1,0.1), col="black", lwd = 1)
      lines(c(-0.5,-0.5), c(-0.1,0.1), col="black", lwd = 1)
    }
    else if(input$corrplottype == "oblique" ){
      lines(c(-1,1), c(-1,-1), col="black", lwd = 2)
      lines(c(-1,1), c(-0.5,-0.5), col="black", lwd = 1)
      lines(c(-1,1), c(0,0), col="black", lwd = 3)
      lines(c(-1,1), c(0.5,0.5), col="black", lwd = 1)
      lines(c(-1,1), c(1,1), col="black", lwd = 2)
      
      lines(c(-1,-1), c(-1,1), col="black", lwd = 2)
      lines(c(-0.5,-0.5), c(-1,1), col="black", lwd = 1)
      lines(c(0,0), c(-1,1), col="black", lwd = 3)
      lines(c(0.5,0.5), c(-1,1), col="black", lwd = 1)
      lines(c(1,1), c(-1,1), col="black", lwd = 3)
    }


  } )

  
    ## Correaltion plot ##
  output$corrprimary <- renderPlot({
    X = projected.primary() 

    cols = rep("#00000050", nrow(X) )
    
    par(mar=c(0.5,0.5,0.5,0.5) )
    pointtype="|"
    plot( y ~ x, data=X, type="p", xlim = c( -1.1, 1.1 ), ylim = c( -1.1, 1.1 ), 
          pch=pointtype, col=cols, bty="n", axes=FALSE, xlab="", ylab="" )
    
    #if( length(input$table_rows_selected) > 0 ){
    #  Sp <- cbind( sliced()$scores[ input$table_rows_selected, ] %*% primary.sliced()$score, rep(0, length(input$table_rows_selected) )  )
    #  if(input$absolute){
    #    Sp = abs(Sp)
    #  }
#
#      points(Sp, pch=19, col="#0000FF")
#    }

    if(input$absolute){
      points( x = abs( c( 1, sum(primary.sliced()$score * secondary.sliced()$score) ) ), y = rep(0,2) , pch=19, col=c("red", "orange") )
    }
    else{
      points( x = c( 1, sum(primary.sliced()$score * secondary.sliced()$score) ), y = rep(0,2) , pch=19, col=c("red", "orange") )
    }

    lines(c(-1,1), c(0,0), col="black", lwd = 3)
    lines(c(0,0), c(-0.09,0.09), col="black", lwd = 2)
    lines(c(1,1), c(-0.12,0.12), col="black", lwd = 2)
    lines(c(-0.5,-0.5), c(-0.06,0.06), col="black", lwd = 1)
    lines(c(0.5,0.5), c(-0.06,0.06), col="black", lwd = 1)
    lines(c(-1,-1), c(-0.12,0.12), col="black", lwd = 2)

  })


    ## Correaltion plot ##
  output$corrsecondary <- renderPlot({
    par(mar=c(0.5,0.5,0.5,0.5) )
    X = projected.secondary()


    cols = rep("#00000050", nrow(X) )

    pointtype="|"
    plot( y~x,  data=X,  type="p", xlim = c( -1.1, 1.1 ), ylim = c( -1.1, 1.1 ), 
          pch=pointtype, col=cols, bty="n", axes=FALSE, xlab="", ylab="" )
    
    #if( length(input$table_rows_selected) > 0 ){
    #  Sp <- cbind( sliced()$scores[ input$table_rows_selected, ] %*% secondary.sliced()$score, rep(0, length(input$table_rows_selected) ) )
     # if(input$absolute){
     #   Sp = abs(Sp)
     # }
     # points(Sp, pch=19, col="#0000FF")
    #}

    if(input$absolute){
      points( x = abs(  c( sum( primary.sliced()$score * secondary.sliced()$score), 1 ) ), 
              y = rep(0,2), pch=19, col=c("red", "orange") )
    }
    else{
      points( x = c( sum( primary.sliced()$score * secondary.sliced()$score), 1 ), 
              y = rep(0,2), pch=19, col=c("red", "orange") )
    }

    lines(c(-1,1), c(0,0), col="black", lwd = 3)
    lines(c(0,0), c(-0.09,0.09), col="black", lwd = 2)
    lines(c(1,1), c(-0.12,0.12), col="black", lwd = 2)
    lines(c(-0.5,-0.5), c(-0.06,0.06), col="black", lwd = 1)
    lines(c(0.5,0.5), c(-0.06,0.06), col="black", lwd = 1)
    lines(c(-1,-1), c(-0.12,0.12), col="black", lwd = 2)

  })
  





  #Table output for slected points
  output$table <- renderDataTable({
    sel <- gmra.index.scores( unique( c(selection$brushed.primary, selection$brushed.secondary) ) )
    corp <- sliced()$scores[sel, ]  %*% primary.sliced()$score
    cors <- sliced()$scores[sel, ]  %*% secondary.sliced()$score
    words <- rownames( sliced()$scores )
    A <- cbind(words[ sel ], round(corp, 2), round(cors,2) )
    colnames(A) <- c("word", "cor( p )", "cor( s )" )
    DT::datatable(A)
  
  })

  output$derivtable <- renderDataTable({
    words <- rownames( sliced()$scores )
    tryCatch({
    A <- cbind(words[ selection$derivs$index ], signif(selection$derivs$vals, 3) )
    colnames(A) <- c("word", "derivs" )
    DT::datatable(A)
    })
  
  })

  output$sderivtable <- renderDataTable({
    tryCatch({
    words <- rownames( sliced()$scores )

    A <- cbind(words[ selection$derivs.scaled$index ], signif(selection$derivs.scaled$vals, 3) )
    colnames(A) <- c("word", "derivs" )
    DT::datatable(A)
    })

  })


  output$wavtable <- renderDataTable({
    tryCatch({
    words <- rownames( sliced()$scores )
    A <- cbind( words[ selection$wavs$index ], signif(selection$wavs$value,3) )
    colnames(A) <- c("word", "coeff" )
    DT::datatable(A)
    })
  
  })


  output$graph.score <- renderPlot({
    sel <- raw.index.scores( unique( c(selection$brushed.primary, selection$brushed.secondary) )  )
    
    X <- smoothed()$scaled
    X <- t(scale(t(X) ) ) / sqrt(ncol(X)-1)  
    years <- smoothed()$years

    yMax = max(X)
    yMin = min(X)
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
    p <- p - mean(p)
    p <- p / sqrt(sum(p^2))
    plot( x=years, p, ylim=c(yMin, yMax), 
          xlim=c(minYear, maxYear), col="red", type="l", 
          bty="n", xlab="years", ylab="counts", lwd=3 )
    
    s <- secondary.smoothed()
    s <- s - mean(s)
    s <- s / sqrt( sum(s^2) ) 
    lines(x=years, s, col="orange", lwd=3)





    if(length(sel) > 0 ){
      lines(x=years, meanC, col="#00000060", lwd=2)
      lines(x=years, (meanC + sdC), col="#00000060", lwd=2)
      #tmp <- meanC - sdC
      #tmp[tmp<0] = 0
      lines(x=years, meanC - sdC, col="#00000060", lwd=2)
    }
    
    sel  = input$table_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="blue", lwd=2)
      }
    }

    sel  = input$sderivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="forestgreen", lwd=2)
      }
    }  

    sel  = input$derivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="olivedrab1", lwd=2)
      }
    }  

    sel  = input$wavtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="deeppink3", lwd=2)
      }
    }     

    if( !is.null(smoothed.derivative()$positive) ){
      abline(v = selection$derivYear, lwd=3, col="darkolivegreen3") 
      dmax <- max(abs( c(smoothed.derivative()$positive.scaled, smoothed.derivative()$negative.scaled) ) )
      points(smoothed.derivative()$years, smoothed.derivative()$positive.scaled/dmax, pch=19, col="snow3")
      points(smoothed.derivative()$years, abs(smoothed.derivative()$negative.scaled/dmax), pch=19, col="snow4")
    }


    if( length(selection$years$start) > 0 ){
      for(i in 1:length(selection$years$start) ){
        abline( v = selection$years$start[i], lwd=3 )
        abline( v = selection$years$end[i], lwd=3 )
        lines( c( selection$years$start[i], selection$years$end[i] ), c(1, 1) )
      }
    }

  })


  #Scaled timeline smoothed
  output$graph.scaled <- renderPlot({
    sel <- raw.index.scores( unique( c(selection$brushed.primary, selection$brushed.secondary) )  )
    
    X <- smoothed()$scaled
    X <- t(apply(X, 1, function(x)(x-min(x))/(max(x)-min(x)))) 
    years <- smoothed()$years

    yMax = max(X)
    yMin = min(X)
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
    p <- p / max(p)
    plot( x=years, p, ylim=c(0, 1), 
          xlim=c(minYear, maxYear), col="red", type="l", 
          bty="n", xlab="years", ylab="counts", lwd=3 )
    
    s <- secondary.smoothed()
    s <- s - min(s)
    s <- s / max(s) 
    lines(x=years, s, col="orange", lwd=3)





    if(length(sel) > 0 ){
      lines(x=years, meanC, col="#00000060", lwd=2)
      lines(x=years, (meanC + sdC), col="#00000060", lwd=2)
      tmp <- meanC - sdC
      #tmp[tmp<0] = 0
      lines(x=years, tmp, col="#00000060", lwd=2)
    }
    
    sel  = input$table_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="blue", lwd=2)
      }
    }

    sel  = input$sderivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="forestgreen", lwd=2)
      }
    }  

    sel  = input$derivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="olivedrab1", lwd=2)
      }
    }  

    sel  = input$wavtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="deeppink3", lwd=2)
      }
    }     

    if( !is.null(smoothed.derivative()$positive) ){
      abline(v = selection$derivYear, lwd=3, col="darkolivegreen3") 
      dmax <- max(abs( c(smoothed.derivative()$positive.scaled, smoothed.derivative()$negative.scaled) ) )
      points(smoothed.derivative()$years, smoothed.derivative()$positive.scaled/dmax, pch=19, col="snow3")
      points(smoothed.derivative()$years, abs(smoothed.derivative()$negative.scaled/dmax), pch=19, col="snow4")
    }


    if( length(selection$years$start) > 0 ){
      for(i in 1:length(selection$years$start) ){
        abline( v = selection$years$start[i], lwd=3 )
        abline( v = selection$years$end[i], lwd=3 )
        lines( c( selection$years$start[i], selection$years$end[i] ), c(1, 1) )
      }
    }

  })


  #Counts timeline smoothed
  output$graph.raw <- renderPlot({
    sel <- raw.index.scores( unique( c(selection$brushed.primary, selection$brushed.secondary) ) )
    #sel <- unique(c(sel, input$derivtable_rows_selected) )
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
      #lines(x=years, meanC, col="#00000060", lwd=2)
      #lines(x=years, (meanC + sdC), col="#00000060", lwd=2)
      #tmp <- meanC - sdC
      #tmp[tmp<0] = 0
      #lines(x=years, tmp, col="#00000060", lwd=2)
    }

    sel  = input$table_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="blue", lwd=2)
      }
    }
    sel  = input$sderivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="forestgreen", lwd=2)
      }
    } 
    
    sel  = input$derivtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="olivedrab1", lwd=2)
      }
    } 

    sel  = input$wavtable_rows_selected
    if( length(sel) > 0 ){
      for(i in 1:length(sel) ){
        lines(x=years, X[sel[i], ], col="deeppink3", lwd=2)
      }
    }     

    if( !is.null(smoothed.derivative()$positive) ){
      abline(v = selection$derivYear, lwd=3, col="darkolivegreen3") 
      dmax <- max(abs( c(smoothed.derivative()$positive, smoothed.derivative()$negative) ) ) / maxC
      points(smoothed.derivative()$years, smoothed.derivative()$positive/dmax, pch=19, col="snow3")
      points(smoothed.derivative()$years, abs(smoothed.derivative()$negative/dmax), pch=19, col="snow4")
    }

    if( length(selection$years$start) > 0 ){
      for(i in 1:length(selection$years$start) ){
        abline( v = selection$years$start[i], lwd=3)
        abline( v = selection$years$end[i], lwd=3 )
        lines( c( selection$years$start[i], selection$years$end[i] ), c(maxC, maxC) )
      }
    }

  } )




  output$wav.pow <- renderPlot({
    if( !is.null( mean.power.p) ){
      par(mar = c(4,4,0,0) )
      axis.1 <- minYear:maxYear
      axis.2 <- 1:nrow(mean.power.p)
      lwd.axis <- 0.25
      n.levels <- 100
      key.cols <- heat.colors(n.levels)
      wavelet.levels = quantile(mean.power.p, probs = seq(from = 0, 
                                                   to = 1, length.out = n.levels + 1))
      image( axis.1, axis.2, t(mean.power.p), col = key.cols, breaks = wavelet.levels, 
            useRaster = TRUE, ylab = "log(Period)", xlab = "Year", axes = TRUE )

      
    }
  })

    
  ob.wav.click <- observeEvent(input$wav_click, {
    if( !is.null(mean.power.p) & !is.null(input$wav_click) ){
      ev = input$wav_click
      axis.1 <- minYear:maxYear
      axis.2 <- 1:nrow(mean.power.p)
      i <- which.min( abs(ev$x - axis.1) )
      j <- which.min( abs(ev$y - axis.2) )
      selection$wavs <<- list(index = powers.index[j,i, ], value = powers.coeff[j, i,] )
    }
  })

}


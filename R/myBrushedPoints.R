myBrushedPoints <- function (df, brush, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
    panelvar2 = NULL) 
{
    if (is.null(brush)) {
       return( NULL )
    }
    if (is.null(brush$xmin)) {
        stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
    }
    use_x <- grepl("x", brush$direction)
    use_y <- grepl("y", brush$direction)
    #xvar <- xvar %OR% brush$mapping$x
    #yvar <- yvar %OR% brush$mapping$y
    #panelvar1 <- panelvar1 %OR% brush$mapping$panelvar1
    #panelvar2 <- panelvar2 %OR% brush$mapping$panelvar2
    keep_rows <- rep(TRUE, nrow(df))
    if (use_x) {
        if (is.null(xvar)) 
            stop("brushedPoints: not able to automatically infer `xvar` from brush")
        x <- shiny:::asNumber(df[[xvar]])
        keep_rows <- keep_rows & (x >= brush$xmin & x <= brush$xmax)
    }
    if (use_y) {
        if (is.null(yvar)) 
            stop("brushedPoints: not able to automatically infer `yvar` from brush")
        y <- shiny:::asNumber(df[[yvar]])
        keep_rows <- keep_rows & (y >= brush$ymin & y <= brush$ymax)
    }
    which(keep_rows)
}

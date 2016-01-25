myNearPoints <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
    panelvar2 = NULL, threshold = 5, maxpoints = NULL) 
{
    if (is.null(coordinfo)) {
      return(NULL)
    }
    if (is.null(coordinfo$x)) {
        stop("nearPoints requires a click/hover/double-click object with x and y values.")
    }
    #xvar <- xvar %OR% coordinfo$mapping$x
    #yvar <- yvar %OR% coordinfo$mapping$y
    #panelvar1 <- panelvar1 %OR% coordinfo$mapping$panelvar1
    #panelvar2 <- panelvar2 %OR% coordinfo$mapping$panelvar2
    if (is.null(xvar)) 
        stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
    if (is.null(yvar)) 
        stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
    x <- shiny:::asNumber(df[[xvar]])
    y <- shiny:::asNumber(df[[yvar]])
    coordPx <- shiny:::scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- shiny:::scaleCoords(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
    keep_rows <- (dists <= threshold)
    keep_idx <- which(keep_rows)
    dists <- dists[keep_idx]
    keep_idx <- keep_idx[order(dists)]
    if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
        keep_idx <- keep_idx[seq_len(maxpoints)]
    }
    keep_idx
}

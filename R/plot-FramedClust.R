
#' Plot Framed Clustering Results
#'
#' Visualize clusters of circular data from clustering result of class \code{CirClust}.
#'
#' @importFrom plotrix draw.circle
#' @importFrom plotrix draw.radial.line
#' @importFrom plotrix arctext
#' @import graphics
#'
#' @param x an object of class as returned by \code{CirClust}
#' @param xlab a character string. The x-axis label for the plot.
#' @param ylab a character string. The x-axis label for the plot.
#' @param main a character string. The title for the plot.
#' @param sub a character string. The subtitle for the plot.
#' @param col.clusters a vector of colors, defined either by integers or by color names. If the length is shorter than the number of clusters, the colors will be reused.
#' @param ... other arguments associated with the plot function
#'
#'
#' @return the same input object of class \code{CirClust}
#'
#'@examples
#'
#' n <- 100
#'
#' m <- 5
#'
#' O <- c(rnorm(n,mean=5,sd=m),rnorm(n,mean=15,sd=m),rnorm(n,mean=26,sd=m))
#'
#' K <- 3
#'
#' Circumference <- 28
#'
#' result <- CirClust(O, K, Circumference, method = "FOCC")
#'
#' color <- c("#0000CD","#808080", "#DC143C")
#'
#' plot(result,col.clusters = color)
#'
#'
#' @export
plot.FramedClust <- function(x,
                          xlab = NULL,
                          ylab = NULL,
                          main = NULL,
                          sub = NULL,
                          col.clusters = NULL,
                          ...)
{
  ck <- x


  if (exists(ck$X_name, mode = "numeric")) {
    X <- get(ck$X_name, mode = "numeric")
  } else {
    X <- eval(parse(text = ck$X_name))
  }

  if (is.null(col.clusters))
  {
    color = c("#009270",
              "#DC143C",
              "#0000CD",
              "#000000",
              "#c902c6",
              "#FA6A03")
  } else{
    color =  col.clusters
  }

  X <- sort(X)

  plot(x=c(min(X),max(X)),y=rep(2,2),xlim = c(min(X),max(X)),ylim = c(0,5),type="n",xlab = "", ylab = "")

  segments(x0 = X, y0 = 0.1, x1 = X, y1 = 1.9, col = "grey")

  rect(xleft = X[( ck$ID + 1 )], ybottom = 0, xright = X[(ck$ID + sum(ck$size) )] , ytop = 2, col = "#fffdd0", border = "black")

  for(i in ( ck$ID + 1 ):( ck$ID + sum(ck$size) ) )
  {

    segments(x0 = X[i], y0 = 0.1, x1 = X[i], y1 = 1.9, col = color[ck$cluster[i] %% length(color) + 1])

  }

  segments(x0 = ck$Border.mid, y0 = -0.125, x1 = ck$Border.mid, y1 = 2.125, col = "black",lty = "dotdash")

  segments(x0 = ck$Border.mid, y0 = -0.125, x1 = ck$Border.mid, y1 = 2.125, col = "black",lty = "dotdash")

  segments(x0 = ck$Border.mid, y0 = -0.125, x1 = ck$Border.mid, y1 = 2.125, col = "black",lty = "dotdash")

}

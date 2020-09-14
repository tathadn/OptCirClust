

#' Plot Circular Clustering Results
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
plot.CirClust <- function(x,
                          xlab = NULL,
                          ylab = NULL,
                          main = NULL,
                          sub = NULL,
                          col.clusters = NULL,
                          ...)
{
  ck <- x
  # if(is.null(xlab)) xlab <- ck$O_name
  # if(is.null(main)) main <- paste("Circular clustering of ",ck$O_name)

  # if(is.null(sub)) sub=paste("n =", length(ck$cluster))

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

  if (exists(ck$O_name, mode = "numeric")) {
    O <- get(ck$O_name, mode = "numeric")
  } else {
    O <- eval(parse(text = ck$O_name))
  }

  # x <- O
  # y <- O


  par(mar = c(0, 0, 0, 0))
  # plot(x, y, type="p",
  #     xlab=xlab, ylab=ylab, main=main, sub=sub)
  plot(
    c(0, 1),
    c(0, 0),
    xlim = c(-1.5, 1.5),
    ylim = c(-1.8, 1.8),
    type = "l",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i"
  )
  draw.circle(0, 0, c(1.15, 0.75), col = c("#fffdd0", "#FFFFFF"))



  draw.radial.line(0, 0.75, c(0, 0), col = c("#000000"))

  arrows(0.35, 0.15, 0.35, 0.55, length = 0.25, angle = 30)

  text(0.65, -0.075, labels = as.character(0))

  Circumference = ck$Circumference


  U <- unique(ck$cluster)

  count <- 1

  clust <- ck$cluster

  for(i in 1:length(U))
  {
    clust[which(ck$cluster == U[i])] <- count

    count <- count + 1
  }


  for (i in 1:length(ck$cluster))
  {
    angle = O[i] / Circumference

    draw.radial.line(0.78,
                     1.12,
                     c(0, 0),
                     angle = (angle * 2 * pi),
                     col = color[clust[i] %% length(color) + 1])


  }




  for (i in 1:length(ck$Border.mid))
  {
    angle = ck$Border.mid[i] / Circumference

    draw.radial.line(
      0.65,
      1.25,
      c(0, 0),
      angle = (angle * 2 * pi),
      col = "#000000",
      lty = "dotdash",
      lwd = 2
    )

  }


  for (i in 1:length(ck$centers))
  {
    angle = ck$centers[i] / Circumference

    arctext(
      paste0("C", i),
      center = c(0, 0),
      radius = 1.25,
      middle = (angle * 2 * pi)
    )

  }

  invisible(ck)

}

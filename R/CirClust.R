## usethis namespace: start
#' @useDynLib OptCirClust, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

#'
#'
#'
#'
#' Circular Data Clustering
#'
#' Perform clustering on circular data to minimize the
#'   within-cluster sum of squared distances.
#'
#' @param O a vector of circular data points. They can be coordinates
#'   along the circle based on distance, or angles around the circle.
#'
#' @param K the number of clusters
#'
#' @param Circumference the circumference of the circle where
#'   data are located
#'
#' @param method the circular clustering method.
#' \code{"FOCC"}: fast and optimal, the default method;
#'   \code{"HEUC"}: based on heuristic k-means, fast but not necessarily optimal;
#'   \code{"BOCC"}: brute-force based on Ckmeans.1d.dp, slow but optimal,
#'   included to provide a baseline.
#'
#'
#'
#' @return A S3 object containing clusters and statistics on the circular data
#'
#'
#' \item{ID}{the starting index of the frame with minimum SSQ}
#'
#' \item{Border}{the borders of \code{K} clusters}
#'
#' \item{cluster}{a vector of clusters assigned to each element in \code{O}.
#' Each cluster is indexed by an integer from 1 to \code{K}.}
#'
#' \item{centers}{a numeric vector of the means for each cluster in the circular data.}
#'
#' \item{withinss}{a numeric vector of the within-cluster sum of squares for each cluster.}
#'
#' \item{size}{a vector of the number of elements in each cluster.}
#'
#' \item{totss}{the total sum of squared distances between each element and the sample mean.
#' This statistic is not dependent on the clustering result.}
#'
#' \item{tot.withinss}{the total sum of within-cluster squared distances between
#'  each element and its cluster mean. This statistic is minimized given the number of clusters.}
#'
#' \item{betweenss}{the	sum of squared distances between each cluster mean and sample mean.
#' This statistic is maximized given the number of clusters.}
#'
#' \item{O_name}{a character string. The actual name of the \code{O} argument.}
#'
#'
#' @examples
#' O <- c(1,2, 10,11,12,13,14,15, 27,28,29,30,31,32, 40,41)
#'
#' K <- 3
#'
#' Circumference <- 42
#'
#' # Perform circular clustering:
#' output <- CirClust(O, K, Circumference)
#'
#' # Visualize the circular clusters:
#' plot(output)
#'
#' @export
#'
CirClust <- function(O,
                     K,
                     Circumference,
                     method = c("FOCC", "HEUC", "BOCC"))
{
  method <- match.arg(method)

  O <- O %% Circumference

  if (FALSE)
  {
    if (!identical(which(O < 0), integer(0)))
    {
      O[which(O < 0)] <- O[which(O < 0)] + Circumference
    }

    if (!identical(which(O > Circumference), integer(0)))
    {
      O[which(O > Circumference)] <-
        O[which(O > Circumference)] - Circumference
    }
  }




  width <- length(O)

  I <- order(O)

  # O_sort <- sort(O, decreasing = FALSE)

  O_sort <- O[I]

  X <- c(O_sort, (O_sort + Circumference))

  First <- 0

  Prev <- -1

  Next <- -1

  Last <- length(X) - width - 1

  if (method == "FOCC")
  {
    result <-
      lin_polylog_framed_clust(X, width, K, First, Last, Prev, Next)

    cluster <-  rep(1, (result$Border[1] - result$ID + 1))


    for (i in 2:K)
    {
      cluster <-
        c(cluster, rep(i, (result$Border[i] - result$Border[i - 1])))
    }

  }
  else if (method == "BOCC")
  {
    result <- quad.framed.clust(X, K, First, Last, width)

    cluster <- result$cluster

    size <- result$size[unique(result$cluster)]

    result$Border <- cumsum(size) + result$ID - 1

  }
  else if (method == "HEUC")
  {
    result <- kmeans.framed.clust(X, K, First, Last, width)

    cluster <- result$cluster

    size <- result$size[unique(result$cluster)]

    result$Border <- cumsum(size) + result$ID - 1
  }










  if (result$ID > 0)
  {
    cluster_new <-
      c(cluster[(width - result$ID + 1):width], cluster[1:(width - result$ID)])    # The result$ID count starts from zero

  }
  else
  {
    cluster_new <- cluster
  }




  cluster[I] <- cluster_new


  if (FALSE)
  {
    count = 1


    while (count <= length(O_sort))
    {
      cor <- which(O %in% O_sort[count])



      cluster[cor] <- cluster_new[count]
      count = count + length(cor)


    }
  }


  result$centers <- result$centers %% Circumference


  if (FALSE)
  {
    if (!identical(which(result$centers > Circumference), integer(0)))
    {
      result$centers[which(result$centers > Circumference)] <-
        result$centers[which(result$centers > Circumference)] - Circumference
    }

    if (!identical(which(result$centers < 0), integer(0)))
    {
      result$centers[which(result$centers < 0)] <-
        result$centers[which(result$centers < 0)] + Circumference
    }
  }



 # result$centers <- sort(result$centers, decreasing = FALSE)


  Border.mid <- (X[result$Border + 1] + X[result$Border + 2]) / 2

  O_name <- deparse(substitute(O))

  result_new <-
    list(
      "cluster" = cluster,
      "centers" = result$centers,
      "withinss" = result$withinss,
      "size" = result$size,
      "totss" = result$totss,
      "tot.withinss"  = result$tot.withinss,
      "betweenss" = result$betweenss,
      "ID" = result$ID,
      "Border" = result$Border,
      "Border.mid" = Border.mid,
      "O_name" = O_name,
      "Circumference" = Circumference
    )

  class (result_new) <- "CirClust"

  return(result_new)
}

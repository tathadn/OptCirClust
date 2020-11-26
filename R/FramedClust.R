
#'
#'
#'
#' Framed Data Clustering
#'
#' Find a frame of given size, among all possible such frames
#' on the input data, to minimize the minimum within-cluster
#' sum of square distances.
#'
#'
#'
#'
#' @param X The set of data points on which the search has to be conducted
#' @param K The Number of Clusters in each frame
#' @param first.frame Starting index of the first frame to be clustered (start counting from 0)
#' @param last.frame Starting index of the last frame to be clustered (start counting from 0)
#' @param frame.width The Number of Points in a frame
#' @param method the circular clustering method.
#' \code{"linear.polylog"}: fast and optimal, the default method;
#'   \code{"kmeans"}: based on heuristic k-means, fast but not necessarily optimal;
#'   \code{"Ckmeans.1d.dp"}: brute-force based on Ckmeans.1d.dp, slow but optimal,
#'   included to provide a baseline.
#'
#'
#' @details The method option `linear.polylog` performs
#' fast optimal framed clustering. The method option
#'  `Ckmeans.1d.dp` performs optimal framed clustering by repeatedly
#'  finding the best clustering within each frame.
#'  The `kmeans` option uses heuristic k-means algorithm in all
#'  frames without guarantee of clustering optimality.
#'
#'
#' @return A dataframe containing important statistics
#' of associated with best frame
#'
#' \item{cluster}{ A vector of clusters assigned to each element in x. Each cluster is indexed by an integer from 1 to k.}
#'
#' \item{centers}{ A numeric vector of the  means for each cluster in the frame.}
#'
#' \item{withinss}{	A numeric vector of the  within-cluster sum of squares for each cluster.}
#'
#' \item{size}{	A vector of the  number of elements in each cluster.}
#'
#' \item{totss}{	Total sum of  squared distances between each element and the sample mean. This statistic is not dependent on the clustering result.}
#'
#' \item{tot.withinss}{	 Total sum of  within-cluster squared distances between each element and its cluster mean. This statistic is minimized given the number of clusters.}
#'
#' \item{betweenss}{	 Sum of  squared distances between each cluster mean and sample mean. This statistic is maximized given the number of clusters.}
#'
#' \item{ID}{Starting index of the frame with minimum SSQ.}
#'
#' \item{Border}{ The cluster border of K clusters.}
#'
#' best.frame -> ID
#'
#' Clearly define Border.
#'
#'@examples
#' X <- c(1:100)
#'
#' K <- 5
#'
#' frame.width <- 10
#'
#' first.frame <- 1
#'
#' last.frame <- 90
#'
#' method <- "linear.polylog"
#'
#' result <- FramedClust( X, K, frame.width, first.frame, last.frame, method)
#'
#' @export


FramedClust <- function(
  X, K, frame.width,
  first.frame = 1,
  last.frame = length(X)-frame.width,
  method = c("linear.polylog", "kmeans", "Ckmeans.1d.dp")

)
{
  I <- order(X)

  X_sort <- X[I]

  prev_k_f = -1
  next_k_f = -1


  if(method == "linear.polylog")
  {
    if(K > frame.width)
    {warning("Number of clusters is greater than the frame width", call. = FALSE)}

    result <- lin_polylog_framed_clust(as.double(X_sort), as.integer(K), as.integer(frame.width), as.integer(first.frame-1), as.integer(last.frame-1), as.integer(prev_k_f), as.integer(next_k_f))

    cluster <-  rep(1, (result$Border[1] - result$ID + 1))

if(K > 1)
{
  for (i in 2:K)
  {
    cluster <-
      c(cluster, rep(i, (result$Border[i] - result$Border[i - 1])))
  }

  cluster_new <- matrix( 0, nrow = 1, ncol = length(X_sort))


  cluster_new[result$ID:(result$ID + frame.width)] <- cluster


  cluster[I] <- cluster_new


  cluster[which(cluster==0)] <- NA

}


  }else if(method == "kmeans"){

    result <- kmeans.framed.clust(X_sort, K, frame.width, first.frame, last.frame)

    cluster <- result$cluster

    cluster_new <- matrix( 0, nrow = 1, ncol = length(X_sort))


    cluster_new[result$ID:(result$ID + frame.width)] <- cluster


    cluster[I] <- cluster_new

    cluster[which(cluster==0)] <- NA

  }else if(method == "Ckmeans.1d.dp"){

    result <- quad.framed.clust(X_sort, K, frame.width, first.frame, last.frame)

    cluster <- result$cluster

    cluster_new <- matrix( 0, nrow = 1, ncol = length(X_sort))


    cluster_new[result$ID:(result$ID + frame.width)] <- cluster


    cluster[I] <- cluster_new

    cluster[which(cluster==0)] <- NA

    }

  Border.mid <- (X[result$Border + 1] + X[result$Border + 2]) / 2

  X_name <- deparse(substitute(X))

  df <-
    list(
      "cluster" = cluster,
      "centers" = result$centers + 1,
      "withinss" = result$withinss,
      "size" = result$size,
      "totss" = result$totss,
      "tot.withinss" = result$tot.withinss,
      "betweenss" = result$betweenss,
      "X_name" = X_name,
      "ID" = result$ID,
      "Border.mid" = Border.mid
    )

  class (df) <- "FramedClust"

  return(df)
}


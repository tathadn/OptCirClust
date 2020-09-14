# quad-framed-clust.R
#
# Created: July, 2020

#' Quadratic Framed Data Clustering
#'
#' Perform framed clustering on linear data with guaranteed
#'   optimality in quadratic time of the sample size.
#'
#' @importFrom Ckmeans.1d.dp Ckmeans.1d.dp
#'
#' @param X The set of data points on which the search has to be conducted
#' @param K The Number of Clusters in each frame
#' @param first.frame Starting index of the first frame to be clustered (start counting from 0)
#' @param last.frame Starting index of the last frame to be clustered (start counting from 0)
#' @param frame.width The Number of Points in a frame
#'
#' @return A dataframe containing important statistics of associated with best frame
#'
#'
#' \item{ID}{Starting index of the frame with minimum SSQ}
#'
#' \item{Border}{ The cluster border of K clusters}
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
#'
#'
#'@examples
#'
#' X <- cos((-10:10))
#'
#' K <- 2
#'
#' first.frame <- 0
#'
#' last.frame <- 0
#'
#' frame.width <- 21
#'
#' output <- quad.framed.clust(X, K, first.frame, last.frame, frame.width)
#'
#' @export

quad.framed.clust <-
  function(X, K, first.frame, last.frame, frame.width)
  {

    X <- sort(X)

    result <- Ckmeans.1d.dp(X[1:frame.width], K, method = "linear")

    value <- result$tot.withinss

    ID <- 0

    size <- result$size[unique(result$cluster)]

    Border <- cumsum(size) - 1


    first.frame <- first.frame + 1

    last.frame <- last.frame + 1

    if ((first.frame + 1) <= last.frame)
    {
      for (i in (first.frame + 1):last.frame)
      {
        points = c(X[i:(i + frame.width - 1)])

        result_new <- Ckmeans.1d.dp(points, K, method = "linear")

        if (result_new$tot.withinss < value)
        {
          value <- result_new$tot.withinss

          ID <- i - 1

          size <- result$size[unique(result$cluster)]

          Border <- cumsum(size) + ID - 1

          result <- result_new
        }
      }
    }

    df <-
      list(
        "cluster" = result$cluster,
        "centers" = result$centers,
        "withinss" = result$withinss,
        "size" = result$size,
        "totss" = result$totss,
        "tot.withinss" = value,
        "betweenss" = result$betweenss,
        "ID" = ID,
        "Border" = Border
      )

    return(df)
  }

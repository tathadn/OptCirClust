% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeans-framed-clust.R
\name{kmeans.framed.clust}
\alias{kmeans.framed.clust}
\title{Heuristic Framed Data Clustering}
\usage{
kmeans.framed.clust(X, K, first.frame, last.frame, frame.width)
}
\arguments{
\item{X}{The set of data points on which the search has to be conducted}

\item{K}{The Number of Clusters in each frame}

\item{first.frame}{Starting index of the first frame to be clustered (start counting from 0)}

\item{last.frame}{Starting index of the last frame to be clustered (start counting from 0)}

\item{frame.width}{The Number of Points in a frame}
}
\value{
A dataframe containing important statistics of associated with best frame


\item{ID}{Starting index of the frame with minimum SSQ.}

\item{Border}{ The cluster border of K clusters.}

\item{cluster}{ A vector of clusters assigned to each element in x. Each cluster is indexed by an integer from 1 to k.}

\item{centers}{ A numeric vector of the  means for each cluster in the frame.}

\item{withinss}{	A numeric vector of the  within-cluster sum of squares for each cluster.}

\item{size}{	A vector of the  number of elements in each cluster.}

\item{totss}{	Total sum of  squared distances between each element and the sample mean. This statistic is not dependent on the clustering result.}

\item{tot.withinss}{	 Total sum of  within-cluster squared distances between each element and its cluster mean. This statistic is minimized given the number of clusters.}

\item{betweenss}{	 Sum of  squared distances between each cluster mean and sample mean. This statistic is maximized given the number of clusters.}
}
\description{
Perform framed clustering on linear data points by repeatedly
 applying the heuristic k-means algorithm in all frames
}
\examples{
X <- cos((-10:10))

K <- 2

first.frame <- 0

last.frame <- 0

frame.width <- 21

output <- kmeans.framed.clust(X, K, first.frame, last.frame, frame.width)

}

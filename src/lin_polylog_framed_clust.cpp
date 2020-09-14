#include <Rcpp.h>
#include "OC.h"
using namespace Rcpp;

//'
//'Fast and Optimal Framed Clustering
//'
//' The data given by vector \code{X} are clustered frame-by-frame.
//' Within each frame, the points are partitioned into k clusters
//' such that the sum of squares from points to the
//' assigned cluster centres is minimized.
//' At the minimum, all cluster centres are at the
//' mean of their Voronoi sets (the set of data points
//' which are nearest to the cluster centre).
//'
//' For ease of programmatic exploration, k=1 is allowed, notably returning the center and withinss.
//'
//' Perform framed data clustering with guaranteed optimality
//'   in linear-polylogarthmic time. The run time of the algorithm to find K optimal clusters in a circular data having N points is O(K N (log N)^2 ).
//'
//' @param X a vector of data points to be clustered
//' @param width the number of points in each frame
//' @param K the number of clusters
//' @param First the index to the start point in the first frame
//' @param Last the index to the start point of the last frame
//' @param Prev the index to the start point of an already clustered frame to the left of First
//' @param Next the index to the start point of an already clustered frame to the right of Last
//'
//' @return A dataframe containing important statistics associated with best frame
//'
//' \item{ID}{Starting index of the frame with minimum SSQ}
//'
//' \item{Border}{ The cluster border of K clusters}
//'
//' \item{cluster}{ A vector of clusters assigned to each element in x. Each cluster is indexed by an integer from 1 to k.}
//'
//' \item{centers}{ A numeric vector of the  means for each cluster.}
//'
//' \item{withinss}{	A numeric vector of the  within-cluster sum of squares for each cluster.}
//'
//' \item{size}{	A vector of the  number of elements in each cluster.}
//'
//' \item{totss}{	Total sum of  squared distances between each element and the sample mean. This statistic is not dependent on the clustering result.}
//'
//' \item{tot.withinss}{	 Total sum of  within-cluster squared distances between each element and its cluster mean. This statistic is minimized given the number of clusters.}
//'
//' \item{betweenss}{	 Sum of  squared distances between each cluster mean and sample mean. This statistic is maximized given the number of clusters.}
//'
//'
//'
//' @examples
//' X <- cos((-10:10))
//'
//' width <- 21
//'
//' K <- 2
//'
//' First <- 0
//'
//' Last <- 0
//'
//' Prev <- -1
//'
//' Next <- -1
//'
//' lin_polylog_framed_clust(X, width, K, First, Last, Prev, Next)
//'
//' @export
// [[Rcpp::export]]
Rcpp::List lin_polylog_framed_clust(
    std::vector<double> & X,
    int width, int K,
    int First, int Last,
    int Prev, int Next)
{
  struct clustering cluster = FOC( X, width,  K, First,  Last, Prev,  Next);
  Rcpp::List result;


  // result["ssq"] = frame.ssq * Data_Points[Data_Points.size() - 1] * Data_Points[Data_Points.size() - 1];
  //result["ssq"] = frame.ssq * scale;
  // result["ssq"] = frame.ssq ;


  result["ID"] = cluster.Frame_ID;

  result["Border"] = cluster.Borders;

  result["centers"] = cluster.centers;

  result["withinss"] = cluster.withinss;

  result["size"] = cluster.size;

  result["totss"] = cluster.totss;

  result["tot.withinss"] = cluster.ssq  ;

  result["betweenss"] = cluster.totss - cluster.ssq;




  /*
   for(int i=0;i<width;i++)
   {
   cout <<endl;
   cout << i << endl;
   cout<< endl;
   for(int j=0;j<K;j++)
   {
   cout << Cluster_Border[i][j] << endl;
   }
   cout << endl;}

   */




  return(result);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
# timesTwo(42)
*/

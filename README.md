The 'OptCirClust' R package
=================================

### Overview

The package provides fast, optimal, and reproducible clustering on circular data by divide-and-conquer and dynamic programming. On a desktop computer using a single processor core, millions of circular data points can be clustered within seconds. Three algorithms are provided to solve the circular clustering problems by minimizing the within-cluster sum of squared distances. In contrast to heuristic clustering, the efficiency and accuracy of fast optimal circular clustering is evident both theoretically and practically. The core algorithm can also be applied to cluster linear periodic data. A plot function visualizes circular clusters to reveal patterns in data. 

### The main method

The function to perform fast optimal cicular data clustering is `CirClust(, method="FOCC")`, which calls the `lin_polylog_framed_clust` function, the engine to perform fast optimal framed linear clustering. Using divide-and-conquer inside dynamic programming into another divide-and-conquer strategy, it guarantees the optimality of clustering---the total of within-cluster sums of squared distances is always the minimum given the number of clusters $K$. 

The `CirClust(, method="FOCC")` function has a runtime of $O(K N \log^2N)$, linear polylogarithmic in sample size $N$ and linear in the number of clusters $K$. The space complexity of the function is $O(KN)$. Its implementation addressed numerical precision issues when the input data have large absolute values.

### When to use the package

To find optimal clustering on circular or linear periodic data, this package provides a first high-performance general solution---efficient, reproducible, and with guaranteed optimality.

### To download and install the package
```{r}
install.packages("OptCirClust")
```

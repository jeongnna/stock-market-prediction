# Args
A <- cor(t(res))
n <- 3


# Construct eigenvector matrix
P <- sign(eigen(A)$vec)
P <- P[, 1:n]


# Clustering
stocks <- 1:nstock
cluster_list <- list()

while (!is_empty(stocks)) {
    cluster <- NULL
    s <- stocks[1]
    
    for (j in stocks) {
        if (all(P[s,] * P[j,] == 1)) {
            cluster <- union(cluster, j)
            stocks <- setdiff(stocks, j)
        }
    }
    
    cluster_list <- append(cluster_list, list(cluster))
}

cluster_list
length(cluster_list)

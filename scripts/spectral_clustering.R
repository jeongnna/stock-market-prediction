# Args
A <- NULL
k <- 3


# Spectral analysis
n <- nrow(A)
D <- diag(apply(A, 1, sum))
sp <- eigen(D - A)
V <- sp$vectors


# k-means
cluster <- kmeans(V[-(1:(n - k))], centers=k)
table(cluster$cluster)

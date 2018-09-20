library(tidyverse)
library(data.table)
library(igraph)
library(bit64)
library(readxl)
library(sandwich)
library(lmtest)


# Functions ---------------------------------------------------------------

dist_mat <- function(X, sigma=1) {
  n = ncol(X)
  mat = matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        mat[i, j] = 0
      } else {
        dist = X[, i] - X[, j]
        mat[i, j] = exp(-(dist %*% dist) / (2 * sigma^2))
      }
    }
  }
  return(mat)
}

row_normalize <- function(X) {
  D = diag(apply(X * X, 1, sum))
  D = sqrt(D)
  return(solve(D) %*% X)
}

internal_test <- function(data, stock_list, cluster_list) {
  
  ts_df <- 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  
  t_table <- matrix(byrow=TRUE, ncol=5)
  colnames(t_table) <- c('n_stocks', 'avg_corr', 'true', 'false', 'false_ratio')
  for (i in 1:length(cluster_list)) {
    cluster <- stock_list[cluster_list[[i]]]
    
    # weight
    weight <- ts_df$ts[ts_df$code %in% cluster]
    names(weight) <- stock_list[cluster_list[[i]]]
    x <- apply(as.matrix(res[, cluster]), 1, function(x) weighted.mean(x, weight))
    
    # test
    pval <- NULL
    corr <- NULL
    for (j in 1:length(cluster)) {
      y <- res[, cluster[j]]
      m <- floor(0.75 * length(y)^(1/3))
      nw_vcov <- NeweyWest(lm(y ~ x), lag=m-1, prewhite=FALSE, adjust=TRUE)
      test_result <- coeftest(lm(y ~ x), vcov=nw_vcov)
      pval <- append(pval, test_result[2,4])
      corr <- c(corr, cor(x, y))
    }
    
    n_stocks <- length(cluster)
    avg_corr <- mean(corr)
    n_signif <- sum(pval < 0.05)
    n_corr0 <- n_stocks - n_signif
    null_rate <- round(n_corr0 / n_stocks, 2)
    t_table <- rbind(t_table, c(n_stocks, avg_corr, n_signif, n_corr0, null_rate))
    
  }
  return(t_table[-1, ])
}

internal_test_walktrap <- function(data, stock_list, cluster_list) {
  
  ts_df <- 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  
  t_table <- matrix(byrow=TRUE, ncol=5)
  colnames(t_table) <- c('n_stocks', 'avg_corr', 'true', 'false', 'false_ratio')
  for (i in 1:length(cluster_list)) {
    cluster <- cluster_list[[i]]
    
    # weight
    weight <- ts_df$ts[ts_df$code %in% cluster]
    names(weight) <- cluster_list[[i]]
    x <- apply(as.matrix(res[, cluster]), 1, function(x) weighted.mean(x, weight))
    
    # test
    pval <- NULL
    corr <- NULL
    for (j in 1:length(cluster)) {
      y <- res[, cluster[j]]
      m <- floor(0.75 * length(y)^(1/3))
      nw_vcov <- NeweyWest(lm(y ~ x), lag=m-1, prewhite=FALSE, adjust=TRUE)
      test_result <- coeftest(lm(y ~ x), vcov=nw_vcov)
      pval <- append(pval, test_result[2,4])
      corr <- c(corr, cor(x, y))
    }
    
    n_stocks <- length(cluster)
    avg_corr <- mean(corr)
    n_signif <- sum(pval < 0.05)
    n_corr0 <- n_stocks - n_signif
    null_rate <- round(n_corr0 / n_stocks, 2)
    t_table <- rbind(t_table, c(n_stocks, avg_corr, n_signif, n_corr0, null_rate))
    
  }
  return(t_table[-1, ])
}

external_test <- function(data, stock_list, cluster_list) {
  
  ts_df <- 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  
  x <- NULL
  for (i in 1:length(cluster_list)) {
    cluster <- stock_list[cluster_list[[i]]]
    
    # weight
    weight <- ts_df$ts[ts_df$code %in% cluster]
    names(weight) <- stock_list[cluster_list[[i]]]
    x <- cbind(x, apply(as.matrix(res[, cluster]), 1, function(x) weighted.mean(x, weight)))
  }
  
  pval_mat <- matrix(nrow=length(cluster_list), ncol=length(cluster_list))
  for (i in 1:length(cluster_list)) {
    for (j in 1:length(cluster_list)) {
      ct <- cor.test(x[,i], x[,j], alternative='greater')
      pval_mat[i, j] <- ct$p.value
    }
  }
  # round(pval_mat, 3)
  return(round(cor(x), 3))
}

external_test_walktrap <- function(data, stock_list, cluster_list) {
  
  ts_df <- 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  
  x <- NULL
  for (i in 1:length(cluster_list)) {
    cluster <- cluster_list[[i]]
    
    # weight
    weight <- ts_df$ts[ts_df$code %in% cluster]
    names(weight) <- cluster_list[[i]]
    x <- cbind(x, apply(as.matrix(res[, cluster]), 1, function(x) weighted.mean(x, weight)))
  }
  
  pval_mat <- matrix(nrow=length(cluster_list), ncol=length(cluster_list))
  for (i in 1:length(cluster_list)) {
    for (j in 1:length(cluster_list)) {
      ct <- cor.test(x[,i], x[,j], alternative='greater')
      pval_mat[i, j] <- ct$p.value
    }
  }
  # round(pval_mat, 3)
  return(round(cor(x), 3))
}

corr_evaluate <- function(data, stock_list, cluster_list) {
  
  ts_df = 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  corr_mat = matrix(nrow=length(cluster_list), ncol=length(cluster_list))

  for (i in 1:length(cluster_list)) {
    cluster_i = stock_list[cluster_list[[i]]]
    
    for (j in 1:length(cluster_list)) {
      cluster_j = stock_list[cluster_list[[j]]]
      weight <- ts_df$ts[ts_df$code %in% cluster_j]
      names(weight) = cluster_j
      x = apply(as.matrix(res[, cluster_j]), 1, function(x) weighted.mean(x, weight))
      
      corr = NULL
      for (k in 1:length(cluster_i)) {
        y = res[, cluster_i[k]]
        corr = c(corr, cor(x, y))
      }
      corr_mat[i, j] = round(mean(corr), 3)
    }
  }
  
  return(corr_mat)
}

t_evaluate <- function(data, stock_list, cluster_list) {
  
  ts_df = 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  t_mat = matrix(nrow=length(cluster_list), ncol=length(cluster_list))
  
  for (i in 1:length(cluster_list)) {
    cluster_i = stock_list[cluster_list[[i]]]
    
    for (j in 1:length(cluster_list)) {
      cluster_j = stock_list[cluster_list[[j]]]
      weight <- ts_df$ts[ts_df$code %in% cluster_j]
      names(weight) = cluster_j
      x = apply(as.matrix(res[, cluster_j]), 1, function(x) weighted.mean(x, weight))
      
      corr = NULL
      for (k in 1:length(cluster_i)) {
        y = res[, cluster_i[k]]
        corr = c(corr, cor(x, y))
      }
      t_mat[i, j] = round(mean(corr*(sqrt(1955))/sqrt(1-corr*corr)),3)
    }
  }
  
  return(t_mat)
}

corr_evaluate_walktrap <- function(data, stock_list, cluster_list) {
  
  ts_df = 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  corr_mat = matrix(nrow=length(cluster_list), ncol=length(cluster_list))
  
  for (i in 1:length(cluster_list)) {
    cluster_i = cluster_list[[i]]
    
    corr = NULL
    for (j in 1:length(cluster_list)) {
      cluster_j = cluster_list[[j]]
      weight <- ts_df$ts[ts_df$code %in% cluster_j]
      names(weight) = cluster_j
      x = apply(as.matrix(res[, cluster_j]), 1, function(x) weighted.mean(x, weight))
      
      corr = NULL
      for (k in 1:length(cluster_i)) {
        y = res[, cluster_i[k]]
        corr = c(corr, cor(x, y))
      }
      corr_mat[i, j] = round(mean(corr), 3)
    }
  }
  
  return(corr_mat)
}

t_evaluate_walktrap <- function(data, stock_list, cluster_list) {
  
  ts_df = 
    data %>% 
    group_by(code) %>% 
    summarize(ts = last(total_shares))
  
  t_mat = matrix(nrow=length(cluster_list), ncol=length(cluster_list))
  
  for (i in 1:length(cluster_list)) {
    cluster_i = cluster_list[[i]]
    
    corr = NULL
    for (j in 1:length(cluster_list)) {
      cluster_j = cluster_list[[j]]
      weight <- ts_df$ts[ts_df$code %in% cluster_j]
      names(weight) = cluster_j
      x = apply(as.matrix(res[, cluster_j]), 1, function(x) weighted.mean(x, weight))
      
      corr = NULL
      for (k in 1:length(cluster_i)) {
        y = res[, cluster_i[k]]
        corr = c(corr, cor(x, y))
      }
      t_mat[i, j] = round(mean(corr*(sqrt(1955))/sqrt(1-corr*corr)),3)
    }
  }
  
  return(t_mat)
}




# Data preperation --------------------------------------------------------

row_data <- fread('data/stock_preprocessed_Copy_3.csv')
row_data$logreturn <- c(NA, diff(log(row_data$closing)))
row_data$return <- exp(row_data$logreturn)

# stock market
KOSPI <- read_excel('data/KOSPI_list.xls')
KOSDAQ <- read_excel('data/KOSDAQ_list.xls')

KOSPI <- paste0('A', KOSPI$code)
KOSDAQ <- paste0('A', KOSDAQ$code)

row_data$market <- 0
row_data$market[row_data$code %in% KOSPI] <- 'kospi'
row_data$market[row_data$code %in% KOSDAQ] <- 'kosdaq'


# market return
kospi <- fread('data/KOSPI.csv')
kosdaq <- fread('data/KOSDAQ.csv')
kospi$logreturn <- c(NA, diff(log(kospi$closing)))
kosdaq$logreturn <- c(NA, diff(log(kosdaq$closing)))


# select full time observated stocks
num_of_each <-
  row_data %>%
  filter(market != 0) %>% 
  group_by(code) %>%
  tally()
stock_list <- unique(num_of_each$code[num_of_each$n == 2075])

timeset <- unique(row_data$time[row_data$code %in% stock_list])
timeset <- intersect(timeset, unique(kospi$time))
timeset <- intersect(timeset, unique(kosdaq$time))

data <- 
  row_data %>% 
  filter(code %in% stock_list,
         market != 0,
         time %in% timeset) %>% 
  select(code, time, logreturn, total_shares, market)

kospi <- kospi %>% filter(time %in% timeset)
kosdaq <- kosdaq %>% filter(time %in% timeset)


# devide training & test sets
train_idx <- year(data$time) < 2018
train <- data[train_idx, ]
test <- data[!train_idx, ]
kospi_tr <- 
  kospi %>% 
  filter(year(time) < 2018)
kosdaq_tr <- 
  kosdaq %>% 
  filter(year(time) < 2018)


# na idx of kospi_tr
na_idx <- is.na(kospi_tr$logreturn) | is.na(kosdaq_tr$logreturn)
table(na_idx)

# Regression to market return ---------------------------------------------

# devide to each stock data
n_stocks <- length(stock_list)
data_list <- list()
for (i in 1:n_stocks) {
  stock <- stock_list[i]
  curr_data <- 
    train %>% 
    filter(code == stock_list[i])
  curr_data$logreturn[1] <- NA
  data_list[[i]] <- curr_data[!na_idx, ]
  names(data_list)[i] <- stock_list[i]
}

# residual return
res <- NULL
for (i in 1:n_stocks) {
  curr_data <- data_list[[i]]
  if (curr_data$market == 'kospi') {
  return_lmfit <- lm(curr_data$logreturn ~ kospi_tr$logreturn[!na_idx])
  residual <- return_lmfit$residuals
  res <- cbind(res, return_lmfit$residuals)
  colnames(res)[i] <- stock_list[i]
  } else {
    return_lmfit <- lm(curr_data$logreturn ~ kosdaq_tr$logreturn[!na_idx])
    residual <- return_lmfit$residuals
    res <- cbind(res, return_lmfit$residuals)
    colnames(res)[i] <- stock_list[i]
  }
}


# Spectral clustering -----------------------------------------------------

# initializing
n <- 5
# construct eigen vector matrix
A <- cor(res)
P <- sign(eigen(A)$vec)
P <- P[, 1:n]

# clustering
stocks <- 1:n_stocks
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

spec_result_list <- list()
case <- c(n)
names(case) <- c('n')
case_result <- 
  list(case = case,
       internal = internal_test(train, stock_list, cluster_list),
       external = external_test(train, stock_list, cluster_list),
       corr_matrix = corr_evaluate(train, stock_list, cluster_list),
       t_matrix=t_evaluate(train, stock_list, cluster_list))
    
spec_result_list <- append(spec_result_list, list(case_result))

spec_result_list


# NJW clustering ----------------------------------------------------------


ns <- 4:7
sigmas <- c(0.3, 0.5, 0.7, 1, 2, 3)
A <- cor(res)
njw_result_list <- list()
for (n in ns) {
  for (sigma in sigmas) {
    
    # construct eigen vector matrix
    
    A <- dist_mat(res, sigma)
    D <- diag(apply(A, 1, sum))
    sD <- solve(D^(1/2))
    L <- sD %*% A %*% sD
    
    P <- eigen(L)$vec
    P <- P[, 1:n]
    Y <- row_normalize(P)
    
    # clustering
    set.seed(123)
    Y_fitted <- fitted(kmeans(Y, n))
    cluster <- as.integer(rownames(Y_fitted))
    
    cluster_list <- list()
    for (i in 1:length(unique(cluster))) {
      cluster_list[[i]] <- which(cluster == i)
    }
    cluster_list
    length(cluster_list)
    
    case <- c(n, sigma)
    names(case) <- c('n', 'sigma')
    case_result <- 
      list(case = case,
           internal = internal_test(train, stock_list, cluster_list),
           external = external_test(train, stock_list, cluster_list),
           corr_matrix = corr_evaluate(train, stock_list, cluster_list),
           t_matrix = t_evaluate(train, stock_list, cluster_list))
    njw_result_list <- append(njw_result_list, list(case_result))
  }
}
njw_result_list

# Walktrap clustering -----------------------

#Obtain Graph
A <- cor(res)
A <- 0.5*(A+abs(A))
P <- graph_from_adjacency_matrix(A, mode="undirected", weighted = TRUE, diag=TRUE)

#Cluster
TrapClust <- cluster_walktrap(P, steps=5, modularity = TRUE, membership = TRUE)

cluster_list <- list()
for (i in 1:length(TrapClust)) {
  cluster_list[i] <- TrapClust[i]
}

length(TrapClust)

walk_result_list <- list()
case <- c(n)
names(case) <- c('n')
case_result <- 
  list(case = case,
       internal = internal_test_walktrap(train, stock_list, cluster_list),
       external = external_test_walktrap(train, stock_list, cluster_list),
       corr_matrix = corr_evaluate_walktrap(train, stock_list, cluster_list),
       t_matrix = t_evaluate_walktrap(train, stock_list, cluster_list))
walk_result_list <- append(walk_result_list, list(case_result))

walk_result_list

# 
# for (i in 1:length(cluster_list)) {
#   file_name <- paste0('walktrap', i, '.csv')
#   write.csv(cluster_list[[i]], file_name, row.names=FALSE)
# }

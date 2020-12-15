## ----eval=FALSE---------------------------------------------------------------
#  EuclideanDistanceSimilarity <- function(M){
#    row <- nrow(M)
#    s <- matrix(0, row, row)
#  
#    for(z1 in 1:row){
#      for(z2 in 1:row){
#        if(z1 < z2){
#          num <- intersect(which(M[z1,] != 0), which(M[z2,] != 0))
#  
#          sum <- 0
#          for(z3 in num){
#            sum <- sum + (M[z1,][z3] - M[z2,][z3])^2
#          }
#  
#          s[z2,z1] <- length(num) / (1+sqrt(sum))
#  
#          if(s[z2, z1] > 1) s[z2,z1] <- 1
#          if(s[z2, z1] < -1) s[z2,z1] <- -1
#        }
#      }
#    }
#  
#    ts <- t(s)
#    w <- which(upper.tri(ts))
#    s[w] <- ts[w]
#    s
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp20070)
data(M, package = "StatComp20070")
S <- EuclideanDistanceSimilarity(M)
S

## ----eval=FALSE---------------------------------------------------------------
#  NearestNUserNeighborhood <- function(S, n){
#    row <- nrow(S)
#    neighbor <- matrix(0, row, n)
#    for(z1 in 1:row){
#      for(z2 in 1:n){
#        m <- which.max(S[,z1])
#        neighbor[z1,][z2] <- m
#        S[,z1][m] = 0
#      }
#    }
#    neighbor
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp20070)
data(S, package = "StatComp20070")
NEIGHBORHOOD_NUM <- 2
N <- NearestNUserNeighborhood(S, NEIGHBORHOOD_NUM)
N

## ----eval=FALSE---------------------------------------------------------------
#  UserBasedRecommender <- function(uid, n, M, S, N){
#    row <- ncol(N)
#    col <- ncol(M)
#    r <- matrix(0, row, col)
#    N1 <- N[uid,]
#    for(z1 in 1:length(N1)){
#      num <- intersect(which(M[uid,] == 0), which(M[N1[z1],]!= 0))
#      for(z2 in num){
#        r[z1,z2] = M[N1[z1],z2]*S[uid, N1[z1]]
#      }
#    }
#  
#    print(r)
#  
#    sum <- colSums(r)
#    s2 <- matrix(0, 2 ,col)
#    for(z1 in 1:length(N1)){
#      num <- intersect(which(colSums(r)!=0), which(M[N1[z1],]!=0))
#      for(z2 in num){
#        s2[1,][z2] <- s2[1,][z2] + S[uid, N1[z1]]
#        s2[2,][z2] <- s2[2,][z2] + 1
#      }
#    }
#  
#    s2[, which(s2[2,] == 1)] = 10000
#    s2 <- s2[-2,]
#  
#    r2 <- matrix(0, n, 2)
#    rr <- sum / s2
#    item <- dimnames(M)[[2]]
#    for(z1 in 1:n){
#      w <- which.max(rr)
#      if(rr[w] > 0.5){
#        r2[z1,1] <- item[which.max(rr)]
#        r2[z1,2] <- as.double(rr[w])
#        rr[w] = 0
#      }
#    }
#    r2
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp20070)
data(M, S, N, package = "StatComp20070")
RECOMMENDER_NUM <- 3
R1 <-UserBasedRecommender(1, RECOMMENDER_NUM, M, S, N)
R1
R2 <- UserBasedRecommender(2, RECOMMENDER_NUM, M, S, N)
R2
R3 <- UserBasedRecommender(3, RECOMMENDER_NUM, M, S, N)
R3
R4 <- UserBasedRecommender(4, RECOMMENDER_NUM, M, S, N)
R4
R5 <- UserBasedRecommender(5, RECOMMENDER_NUM, M, S, N)
R5


#' @title Recommender based on users
#' @name UserBasedRecommender
#' @description Recommend things based on users
#' @param uid user's id
#' @param n the max number of recommender results 
#' @param M the data model
#' @param S S is the similarity matrix of users
#' @param N The Nearest N User Neighborhood
#' @return  the recommend result
#' @examples
#' \dontrun{
#' data(M)
#' attach(M)
#' data(S)
#' attach(S)
#' data(N)
#' attach(N)
#' RECOMMENDER_NUM <- 3
#' R1 <-UserBasedRecommender(1, RECOMMENDER_NUM, M, S, N)
#' R1
#' R2 <- UserBasedRecommender(2, RECOMMENDER_NUM, M, S, N)
#' R2
#' R3 <- UserBasedRecommender(3, RECOMMENDER_NUM, M, S, N)
#' R3
#' R4 <- UserBasedRecommender(4, RECOMMENDER_NUM, M, S, N)
#' R4
#' R5 <- UserBasedRecommender(5, RECOMMENDER_NUM, M, S, N)
#' R5
#' }
#' @export
UserBasedRecommender <- function(uid, n, M, S, N){
  row <- ncol(N)
  col <- ncol(M)
  r <- matrix(0, row, col)
  N1 <- N[uid,]
  for(z1 in 1:length(N1)){
    num <- intersect(which(M[uid,] == 0), which(M[N1[z1],]!= 0))
    for(z2 in num){
      r[z1,z2] = M[N1[z1],z2]*S[uid, N1[z1]]
    }
  }
  
  print(r)
  
  sum <- colSums(r)
  s2 <- matrix(0, 2 ,col)
  for(z1 in 1:length(N1)){
    num <- intersect(which(colSums(r)!=0), which(M[N1[z1],]!=0))
    for(z2 in num){
      s2[1,][z2] <- s2[1,][z2] + S[uid, N1[z1]]
      s2[2,][z2] <- s2[2,][z2] + 1
    }
  }
  
  s2[, which(s2[2,] == 1)] = 10000
  s2 <- s2[-2,]
  
  r2 <- matrix(0, n, 2)
  rr <- sum / s2
  item <- dimnames(M)[[2]]
  for(z1 in 1:n){
    w <- which.max(rr)
    if(rr[w] > 0.5){
      r2[z1,1] <- item[which.max(rr)]
      r2[z1,2] <- as.double(rr[w])
      rr[w] = 0
    }
  }
  r2
}
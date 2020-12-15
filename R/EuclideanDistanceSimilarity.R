#' @title The Euclidean Distance Similarity of Users
#' @name EuclideanDistanceSimilarity
#' @description This function computes the euclidean distance similarity of users.
#' @param M the data model
#' @return s the similarity matrix of users
#' @examples
#' \dontrun{
#' data(M)
#' attach(M)
#' S <- EuclideanDistanceSimilarity(M)
#' }
#' @export
EuclideanDistanceSimilarity <- function(M){
  row <- nrow(M)
  s <- matrix(0, row, row)
  
  for(z1 in 1:row){
    for(z2 in 1:row){
      if(z1 < z2){
        num <- intersect(which(M[z1,] != 0), which(M[z2,] != 0))
        
        sum <- 0
        for(z3 in num){
          sum <- sum + (M[z1,][z3] - M[z2,][z3])^2
        }
        
        s[z2,z1] <- length(num) / (1+sqrt(sum))
        
        if(s[z2, z1] > 1) s[z2,z1] <- 1
        if(s[z2, z1] < -1) s[z2,z1] <- -1
      }
    }
  }
  
  ts <- t(s)
  w <- which(upper.tri(ts))
  s[w] <- ts[w]
  s
}
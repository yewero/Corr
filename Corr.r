
Corr <- function(expr_in) {
  nCell <- nrow(expr_in)
  nGene <- ncol(expr_in)
  corMat <- matrix(NA, nrow = nCell, ncol = nCell)
  rownames(corMat) <- rownames(expr_in)
  colnames(corMat) <- rownames(expr_in)
  
  for(i in 1:nCell) {
    for(j in 1:nCell) {
      #cat(">", nCell, ":", i, j, "\n")
      avgOther <- colMeans(expr_in[- c(i, j), ])
      
      u1 <- rep(0, nGene)
      u1[expr_in[i, ] > avgOther] <- 1
      u1[expr_in[i, ] < avgOther] <- -1
      
      u2 <- rep(0, nGene)
      u2[expr_in[j, ] > avgOther] <- 1
      u2[expr_in[j, ] < avgOther] <- -1
      
      corMat[i, j] <- cor(u1, u2, method = "pearson")
    }
  }
  return(corMat)
}

# set.seed(1)
# expr_test <- matrix(runif(n = 20000), nrow = 10)
# rownames(expr_test) <- paste0("C", 1:nrow(expr_test))
# 
# cor_res <- Corr(expr_in = expr_test)

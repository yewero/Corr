
library("parallel")

Corr <- function(expr_in, ncpu = 1) {
  nCell <- nrow(expr_in)
  nGene <- ncol(expr_in)
  corMat <- matrix(0, nrow = nCell, ncol = nCell)
  rownames(corMat) <- rownames(expr_in)
  colnames(corMat) <- rownames(expr_in)
  
  idx_DF <- data.table::CJ(1:nCell, 1:nCell)[, c(2, 1)]
  colnames(idx_DF) <- c("C1", "C2")
  idx_DF <- subset(idx_DF, C1 > C2)
  
  cl <- makeCluster(ncpu, type = "SOCK")
  cor_value <- parApply(cl, idx_DF, 1, function(x) {
    i <- x[1]
    j <- x[2]
    #cat(">", nCell, ":", i, j, "\n")
    avgOther <- colMeans(expr_in[- c(i, j), ])
    u1 <- rep(0, nGene)
    u1[expr_in[i, ] > avgOther] <- 1
    u1[expr_in[i, ] < avgOther] <- -1
    
    u2 <- rep(0, nGene)
    u2[expr_in[j, ] > avgOther] <- 1
    u2[expr_in[j, ] < avgOther] <- -1
    
    y <- cor(u1, u2, method = "pearson")
    return(y)
  })
  stopCluster(cl); rm(cl)
  
  corMat[lower.tri(corMat)] <- cor_value
  corMat <- corMat + t(corMat)
  diag(corMat) <- 1
  return(corMat)
}

# set.seed(1)
# expr_test <- matrix(runif(n = 20000), nrow = 50)
# rownames(expr_test) <- paste0("C", 1:nrow(expr_test))
# 
# cor_res <- Corr(expr_in = expr_test, ncpu = 5)

Corr_quick <- function(expr_in, ncpu = 1) {
  nCell <- nrow(expr_in)
  nGene <- ncol(expr_in)

  cat("> Calculate comparison matrix...\n")
  cl <- makeCluster(ncpu, type = "SOCK")
  u_MT <- parSapply(cl, 1:nCell, function(i) {
    cat(">", i, "/", nCell, "\n")
    avgOther <- colMeans(expr_in[- i, ])
    u <- rep(0, nGene)
    u[expr_in[i, ] > avgOther] <- 1
    u[expr_in[i, ] < avgOther] <- -1
    return(u)
  })
  stopCluster(cl); rm(cl)
  rownames(u_MT) <- colnames(expr_in)
  colnames(u_MT) <- rownames(expr_in)

  cat("> Calculate correlation...\n")
  corMat <- cor(u_MT, method = "pearson")
  return(corMat)
}

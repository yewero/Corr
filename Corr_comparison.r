
# comparison with matlab code
set.seed(1)
expr_test <- matrix(runif(n = 20000), nrow = 50)
rownames(expr_test) <- paste0("C", 1:nrow(expr_test))
write.table(x = expr_test, file = "expr_test.txt", row.names = F, col.names = F, quote = F, sep = "\t")
cor_res <- Corr(expr_in = expr_test)

expr_in <- expr_test
nCell <- nrow(expr_in)
nGene <- ncol(expr_in)
corMat <- matrix(NA, nrow = nCell, ncol = nCell)
rownames(corMat) <- rownames(expr_in)
colnames(corMat) <- rownames(expr_in)
i <- 1
j <- 5
avgOther <- colMeans(expr_in[- c(i, j), ])
u1 <- rep(0, nGene)
u1[expr_in[i, ] > avgOther] <- 1
u1[expr_in[i, ] < avgOther] <- -1
u2 <- rep(0, nGene)
u2[expr_in[j, ] > avgOther] <- 1
u2[expr_in[j, ] < avgOther] <- -1

###
# run matlab codes
###

cor_res_matlab <- read.table("cor_res_matlab.txt", header = F, sep = "\t")
rownames(cor_res_matlab) <- rownames(cor_res)
colnames(cor_res_matlab) <- colnames(cor_res)

range(unlist(cor_res - cor_res_matlab))

avgOther_matlab <- read.table("m.txt", header = F, sep = "\t", stringsAsFactors = F)[, 1]
u1_matlab <- read.table("u1.txt", header = F, sep = "\t", stringsAsFactors = F)[, 1]
u2_matlab <- read.table("u2.txt", header = F, sep = "\t", stringsAsFactors = F)[, 1]

all(u1 == u1_matlab)
all(u2 == u2_matlab)

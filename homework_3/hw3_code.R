## imports
set.seed(1)
install.packages("ReIns")
install.packages("qqman")
install.packages("BiocManager")
install.packages("lsa")
install.packages("ggplot2")
install.packages("gtools")
BiocManager::install("qvalue")
library("ReIns")
library("qqman")
library("qvalue")
library("lsa")
library("ggplot2")
library("gtools")
## Question 1
qqnorm(movie_votes$AverageVote)
ExpQQ(airport_routes$NumberOfRoutes, plot=TRUE, main="Exponential QQ-plot")
## Question 2
# a)
m1 = matrix(nrow=100, ncol=1001)
for (row in 1:100) {
  for (col in 1:1000) {
    m1[row, col] <- rnorm(1)
  }
}
for (row in 1:100) {
  if ((m1[row, 33] + m1[row, 261] + m1[row, 425] + m1[row, 768] + m1[row, 902]) > 0) {
    m1[row, 1001] = 1
  } else {
    m1[row, 1001] = 0
  }
}
heatmap(m1)
# b)
y = m1[, 1001]
rpbs = matrix(nrow=1000, ncol=4)
for (col in 1:1000) {
  temp_x = m1[,col]
  # calculate pearson correlation
  rpbs[col, 1] <- cor.test(temp_x, y)$p.value
  rpbs[col, 2] <- col
  rpbs[col, 3] <- col
  rpbs[col, 4] <- col
}
rpbs <- as.data.frame(rpbs)
colnames(rpbs) <- c(c('p-value', 'x', 'bp', 'snp'))
manhattan(rpbs, chr='x', p='p-value', bp='bp', snp='snp', xlab="Variable")
# c)
which(rpbs$'p-value' < 0.01)
length(which(rpbs$'p-value' < 0.01))
which(rpbs$'p-value' < (0.01/1000))
length(which(rpbs$'p-value' < (0.01/1000)))
qobj <- qvalue(p=rpbs$'p-value')
which(qobj$'qvalues' < 0.1)
length(which(qobj$'qvalues' < 0.1))
# d)
# permute 1000 times
perm_y = m1[, 1001]
for (i in 1:10000) {
  perm_y = gtools::permute(perm_y)
}
perm_rpbs = matrix(nrow=1000, ncol=4)
for (col in 1:1000) {
  temp_x = m1[,col]
  # calculate pearson correlation
  perm_rpbs[col, 1] <- cor.test(temp_x, perm_y)$p.value
  perm_rpbs[col, 2] <- col
  perm_rpbs[col, 3] <- col
  perm_rpbs[col, 4] <- col
}
perm_rpbs <- as.data.frame(perm_rpbs)
colnames(perm_rpbs) <- c(c('p-value', 'x', 'bp', 'snp'))
which(perm_rpbs$'p-value' < 0.01)
length(which(rpbs$'p-value' < 0.01))
which(perm_rpbs$'p-value' < (0.001))
length(which(perm_rpbs$'p-value' < (0.001)))
hist(perm_rpbs$`p-value`)
## Question 3
pairwise_dist <- matrix(nrow=100, ncol=100)
euclidean_dist <- function(x1, x2) {
  dsq = 0
  for (i in 1:1000) {
    dsq <- dsq + ((x1[i] - x2[i])^2)
  }
  return (sqrt(dsq))
}
for (i in 1:100) {
  x1 <- m1[i, ]
  for (j in 1:100) {
    x2 <- m1[j, ]
    pairwise_dist[i, j] <- euclidean_dist(x1, x2)
  }
}
cosine_dist <- lsa::cosine(t(m1[,-1001]))
euc_vec <- pairwise_dist[lower.tri(pairwise_dist)]
u <- mean(euc_vec)
std <- sd(euc_vec)
# normalization
# new_euc_vec = matrix()
#  (i in 1:4950) {
#   new_euc_vec[i] <- (euc_vec[i] - u) / std
# }
# euc_vec <- new_euc_vec
cos_vec <- cosine_dist[lower.tri(cosine_dist)]
q3df <- matrix(nrow=4950*2, ncol=2)
for (i in 1:(4950*2)) {
  if (i <= 4950) {
    q3df[i, 1] = euc_vec[i]
    q3df[i, 2] = 0
  } else {
    q3df[i, 1] = cos_vec[i-4950]
    q3df[i, 2] = 1
  }
}
q3df <- as.data.frame(q3df)
colnames(q3df) <- c(c('dist', 'type'))
q3df$type <- as.factor(q3df$type)
p <- ggplot(q3df, aes(x=q3df$type, y=q3df$dist)) + 
  geom_violin()
p

## Question 4
table <- matrix(nrow=2, ncol=10)
dist <- matrix(nrow=100, ncol=100)
euclidean_dist_mod <- function(x1, x2, n) {
  dsq = 0
  vars = n
  for (i in 1:vars) {
    dsq <- dsq + ((x1[i] - x2[i])^2)
  }
  return (sqrt(dsq))
}

for (vars in 1:10) {
  for (i in 1:100) {
    x1 <- m1[i, ]
    for (j in 1:100) {
      x2 <- m1[j, ]
      dist[i, j] <- euclidean_dist_mod(x1, x2, 2^(vars-1))
    }
  }
  euc_vec <- dist[lower.tri(dist)]
  u <- mean(euc_vec)
  std <- sd(euc_vec)
  table[1, vars] <- u
  table[2, vars] <- std
}
hist(table[1,], breaks=20)

## Question 5
# a
rownames <- c("For", "Against")
colnames <- c("Low", "Medium", "High")
chitb <- matrix(c(213, 203, 182, 138, 110, 154), nrow=2, dimnames=list(rownames, colnames))
chitb <- as.table(chitb)
chisq <- chisq.test(chitb)
chisq$p.value
# b
rownames1 <- c("poly", "fixed")
colnames1 <- c("syn", "nonsyn")
chitb1 <- matrix(c(43, 17, 5, 7), nrow=2, dimnames=list(rownames1, colnames1))
chitb1 <- as.table(chitb1)
chisq1 <- chisq.test(chitb1)
chisq1$p.value
fisher1 <- fisher.test(chitb1)
fisher1$p.value

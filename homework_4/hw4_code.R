install.packages("factoextra")
install.packages("cluster")
library(factoextra)
library(cluster)
## load data
library(readr)
GeneExpression <- read_csv("G:/CSDS313/homework_4/GeneExpression.csv", col_names = FALSE)

plot(0,0,xlim=c(1, 978),ylim=c(min(as.numeric(unlist(GeneExpression))), max(as.numeric(unlist(GeneExpression)))), xlab='Gene Index', ylab='Expression Level', type="n")
colors=c('red', 'blue', 'orange', 'black', 'green', 'purple', 'cornflowerblue', 'pink', 'grey', 'yellow')
## 4a)
for (sample in 1:10) {
  # visualize
  lines(1:978, GeneExpression[sample,], col=colors[sample], xlab='Gene Index', ylab='Expression Level')
}

## 4b)
set.seed(123)
km.res <- kmeans(GeneExpression, 10, nstart=20)
print(km.res$cluster)
plot(1:300, km.res$cluster, xlab='Sample ID', ylab='Cluster ID')
print(km.res$tot.withinss)

## 4c)
ks = 2:30
sses = matrix(nrow=29, ncol=1)
aics = matrix(nrow=29, ncol=1)
bics = matrix(nrow=29, ncol=1)

#internals
min = 0
max = 0
kmeanStats = function(km){
  
  m = ncol(km$centers)
  n = length(km$cluster)
  k = nrow(km$centers)
  D = km$tot.withinss
  return (data.frame(SSE=km$tot.withinss, AIC = D + 2*m*k, BIC = D + log(n)*m*k))
}
for (k in ks) {
  km.res <- kmeans(GeneExpression, k, nstart=20)
  if (min == 0) {
    min = min(c(kmeanStats(km.res)$SSE, kmeanStats(km.res)$AIC, kmeanStats(km.res)$BIC))
  } else {
    temp = min(c(kmeanStats(km.res)$SSE, kmeanStats(km.res)$AIC, kmeanStats(km.res)$BIC))
    if (temp < min) {
      min = temp
    }
  }
  if (max == 0) {
    max = max(c(kmeanStats(km.res)$SSE, kmeanStats(km.res)$AIC, kmeanStats(km.res)$BIC))
  } else {
    temp = max(c(kmeanStats(km.res)$SSE, kmeanStats(km.res)$AIC, kmeanStats(km.res)$BIC))
    if (temp > max) {
      max = temp
    }
  }
  sses[k-1] <- kmeanStats(km.res)$SSE
  aics[k-1] <- kmeanStats(km.res)$AIC
  bics[k-1] <- kmeanStats(km.res)$BIC
}
print(sses)
print(aics)
print(bics)
plot(0,0,xlim=c(2, 30), ylim=c(min, max), xlab='Cluster Index', ylab='AIC, BIC, SSE', type="n")
points(ks, sses, col='blue')
points(ks, aics, col='red')
points(ks, bics, col='green')
legend(1,max,legend=c("SSE","AIC","BIC"), col=c("blue","red","green"), pch=c("o","o","o"), ncol=1)
print(which.min(aics))
print(which.min(bics))

## 4d)
euclidean <- dist(GeneExpression, method='euclidean', upper=TRUE)
euc_m <- as.matrix(euclidean)
heatmap(euc_m)
Pairwise_Distance <- euc_m[upper.tri(euc_m)]
hist(Pairwise_Distance, xlab='Pairwise Euclidean Distance Between 300 Samples', ylab='Frequency')

## 4e)
correlation <- matrix(1, 300, 300) - cor(t(GeneExpression))
heatmap(correlation)
Pairwise_Correlation <- correlation[upper.tri(correlation)]
hist(Pairwise_Correlation, xlab='Pairwise Correlation Between 300 Samples', ylab='Frequency')

## 4f)
d <- matrix(1, 300, 300) - cor(t(GeneExpression))
heatmap(d)
hc <- agnes(d, method='average')
pltree(hc, cex=0.6, hang=-1, main='Dendrogram of Agglomerative Clustering', xlab='Average Link')

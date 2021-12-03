install.packages("tidyverse")
install.packages("glmnet")
install.packages("ggplot2")
library(ggplot2)
library(glmnet)
library(tidyverse)
# Question 1
m = matrix(nrow=100, ncol=1002)
for (row in 1:100) {
  for (col in 1:1000) {
    m[row, col] <- rnorm(1)
  }
}

for (row in 1:100) {
  sum = m[row, 33] + m[row, 261] + m[row, 425] + m[row, 768] + m[row, 902]
  if (sum > 0) {
    m[row, 1001] = 1
  } else {
    m[row, 1001] = 0
  }
  m[row, 1002] = sum + rnorm(1, mean=0, sd=2)
}

## a (feature filtering)
coefs = matrix(nrow=1000, ncol=2)
for (var in 1:1000) {
  temp_x = m[, var]
  coefs[var, 1] = var
  coefs[var, 2] = abs(cor.test(temp_x, m[, 1002])$estimate)
}
top_coef = coefs[order(coefs[, 2], decreasing=TRUE), ]
top_coef = top_coef[c(1:10),]
idxs = top_coef[,1]
for (idx in 1:10) {
  if (idxs[idx] == 33 || idxs[idx] == 261 || idxs[idx] == 425 || idxs[idx] == 768 || idxs[idx] == 902) {
    print(idxs[idx])
  }
}

## b (multiple linear regression)
varIdx = append(idxs, 1002, after = 10)
dataset = as.data.frame(m[, varIdx])
model = lm(V11 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10, data = dataset)
summary(model)

## c (LASSO regression)
x = data.matrix(dataset[, c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10')])
y = dataset$V11
model_0.1 = glmnet(x, y, alpha = 0.1, lambda = 0.1)
coef(model_0.1)
model_1 = glmnet(x, y, alpha = 0.1, lambda = 1)
coef(model_1)
model_10 = glmnet(x, y, alpha = 0.1, lambda = 10)
coef(model_10)

## d (feature filtering and discretizing)
binM = m
for (i in 1:100) {
  for (j in 1:1000) {
    if (binM[i, j] > 0) {
      binM[i, j] = 1
    } else {
      binM[i, j] = 0
    }
  }
}
scores = matrix(nrow=1000, ncol=2)
for (col in 1:1000) {
  score = 0
  for (row in 1:100) {
    if (binM[row, col] == binM[row, 1001]) {
      score = score + 1
    }
  }
  scores[col, 1] = col
  scores[col, 2] = score/100
}
top_scores = scores[order(scores[, 2], decreasing=TRUE), ]
top_scores = top_scores[c(1:10), ]
idxs = top_scores[,1]
for (idx in 1:10) {
  if (idxs[idx] == 33 || idxs[idx] == 261 || idxs[idx] == 425 || idxs[idx] == 768 || idxs[idx] == 902) {
    print(idxs[idx])
  }
}

### e (Naive Bayesian)
cpt = matrix(nrow=10, ncol=3)
i = 1
for (col in idxs) {
  n0 = 0
  n1 = 0
  for (row in 1:100) {
    score = binM[row, col]
    outcome = binM[row, 1001]
    if (score == 1) {
      if (outcome == 1) {
        n1 = n1 + 1
      } else {
        n0 = n0 + 1
      }
    }
  }
  cpt[i, 1] = col
  cpt[i, 2] = n0
  cpt[i, 3] = n1
  i = i + 1
}

### f (Metrics and LOOCV)
#### functions
accuracy <- function(y_hat, y) {
  t = 0
  for (index in 1:length(y)) {
    if (y[index] == y_hat[index]) {
      t = t + 1
    }
  }
  return(t / length(y))
}
precision <- function(y_hat, y) {
  tp = 0
  fp = 0
  for (index in 1:length(y)) {
    if (y[index] == y_hat[index]) {
      if (y_hat[index] == 1) {
        tp = tp + 1
      }
    } else {
      if (y_hat[index] == 1) {
        fp = fp + 1
      }
    }
  }
  if ((tp + fp) != 0) {
    return(tp / (tp + fp))
  } else {
    return(0)
  }
}
recall <- function(y_hat, y) {
  tp = 0
  fn = 0
  for (index in 1:length(y)) {
    if (y[index] == y_hat[index]) {
      if (y[index] == 1) {
        tp = tp + 1
      }
    } else {
      if (y[index] == 1) {
        fn = fn + 1
      }
    }
  }
  if ((tp + fn) != 0) {
    return(tp / (tp + fn))
  } else {
    return(0)
  }
}
f1 <- function(p, r) {
  if ((p + r) != 0) {
    return(2 * p * r / (p + r))
  }
  return(0)
}
metrics = matrix(nrow=100, ncol=2)
for (loo in 1:100) {
  i = 1
  cpt = matrix(nrow=20, ncol=3)
  for (col in idxs) {
    n00 = 0
    n01 = 0
    n10 = 0
    n11 = 0
    for (row in 1:100) {
      if (loo == row) { next }
      score = binM[row, col]
      outcome = binM[row, 1001]
      if (score == 1) {
        if (outcome == 1) {
          n11 = n11 + 1
        } else {
          n10 = n10 + 1
        }
      } else {
        if (outcome == 1) {
          n01 = n01 + 1
        } else {
          n00 = n00 + 1
        }
      }
    }
    cpt[i, 1] = col
    cpt[i, 2] = n00
    cpt[i, 3] = n01
    cpt[i + 1, 2] = n10
    cpt[i + 1, 3] = n11
    i = i + 2
  }
  #### update cpt to probability
  p_cpt = matrix(nrow=20, ncol=3)
  for (i in c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)) {
    sum0 = cpt[i, 2] + cpt[i + 1, 2]
    sum1 = cpt[i, 3] + cpt[i + 1, 3]
    p_cpt[i, 1] = cpt[i, 1]
    p_cpt[i, 2] = cpt[i, 2] / sum0
    p_cpt[i, 3] = cpt[i, 3] / sum1
    p_cpt[i + 1, 2] = cpt[i + 1, 2] / sum0
    p_cpt[i + 1, 3] = cpt[i + 1, 3] / sum1
  }
  sample = binM[loo, ]
  test = sample[idxs]
  p1 = 0
  p0 = 0
  idx = 1
  for (fidx in 1:10) {
    f_v = test[fidx]
    if (f_v == 0) {
      p0 = log2(p_cpt[idx, 2]) + p0
      p1 = log2(p_cpt[idx, 3]) + p1
    } else {
      p0 = log2(p_cpt[idx + 1, 2]) + p0
      p1 = log2(p_cpt[idx + 1, 3]) + p1
    }
    idx = idx + 2
  }
  pred = 0
  if (p1 >= p0) {
    pred = 1
  } else {
    pred = 0
  }
  metrics[loo, 2] = binM[loo, 1001]
  metrics[loo, 1] = pred
  # metrics[loo, 1] = accuracy(pred, true)
  # metrics[loo, 2] = precision(pred, true)
  # metrics[loo, 3] = recall(pred, true)
  # metrics[loo, 4] = f1(metrics[loo, 2], metrics[loo, 3])
}
#### plot
data = data.frame(
  name=c("accuracy", "precision", "recall", "f1"),
  value=c(mean(metrics[, 1]), mean(metrics[, 2]), mean(metrics[, 3]), mean(metrics[, 4])),
  sd=c(sqrt(var(metrics[, 1])), sqrt(var(metrics[, 2])), sqrt(var(metrics[, 3])), sqrt(var(metrics[, 4])))
)
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

### g (Generalization power, ROC curve, PR curve)
testData = matrix(nrow=50, ncol=1002)
for (row in 1:50) {
  for (col in 1:1000) {
    testData[row, col] <- rnorm(1)
  }
}
for (row in 1:50) {
  sum = testData[row, 33] + testData[row, 261] + testData[row, 425] + testData[row, 768] + testData[row, 902]
  if (sum > 0) {
    testData[row, 1001] = 1
  } else {
    testData[row, 1001] = 0
  }
}
#### discretization
for (i in 1:50) {
  for (j in 1:1000) {
    if (testData[i, j] > 0) {
      testData[i, j] = 1
    } else {
      testData[i, j] = 0
    }
  }
}
#### train on all samples
cpt = matrix(nrow=20, ncol=3)
i = 1
for (col in idxs) {
  n00 = 0
  n01 = 0
  n10 = 0
  n11 = 0
  for (row in 1:100) {
    score = binM[row, col]
    outcome = binM[row, 1001]
    if (score == 1) {
      if (outcome == 1) {
        n11 = n11 + 1
      } else {
        n10 = n10 + 1
      }
    } else {
      if (outcome == 1) {
        n01 = n01 + 1
      } else {
        n00 = n00 + 1
      }
    }
  }
  cpt[i, 1] = col
  cpt[i, 2] = n00
  cpt[i, 3] = n01
  cpt[i + 1, 2] = n10
  cpt[i + 1, 3] = n11
  i = i + 2
}
#### update cpt to probability
p_cpt = matrix(nrow=20, ncol=3)
for (i in c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)) {
  sum0 = cpt[i, 2] + cpt[i + 1, 2]
  sum1 = cpt[i, 3] + cpt[i + 1, 3]
  p_cpt[i, 1] = cpt[i, 1]
  p_cpt[i, 2] = cpt[i, 2] / sum0
  p_cpt[i, 3] = cpt[i, 3] / sum1
  p_cpt[i + 1, 2] = cpt[i + 1, 2] / sum0
  p_cpt[i + 1, 3] = cpt[i + 1, 3] / sum1
}
#### declare metrics helper
roc_helper <- function(py_hat, y) {
  pairs = matrix(nrow=11, ncol=2)
  for (i in 1:11) {
    threshold = (i - 1) / 10
    y_hat = as.integer(py_hat > threshold)
    tp = 0
    tn = 0
    fp = 0
    fn = 0
    for (j in 1:length(y)) {
      if (y_hat[j] == 1 && y[j] == 1) {
        tp = tp + 1
      }
      if (y_hat[j] == 0 && y[j] == 0) {
        tn = tn + 1
      }
      if (y_hat[j] == 1 && y[j] == 0) {
        fp = fp + 1
      }
      if (y_hat[j] == 0 && y[j] == 1) {
        fn = fn + 1
      }
    }
    fpr = fp / (fp + tn)
    tpr = tp / (tp + fn)
    pairs[i, 1] = fpr
    pairs[i, 2] = tpr
  }
  return(pairs)
}
pr_helper <- function(py_hat, y) {
  pairs = matrix(nrow=11, ncol=2)
  for (i in 1:11) {
    threshold = (i - 1) / 10
    y_hat = as.integer(py_hat > threshold)
    pairs[i, 1] = recall(y_hat, y)
    pairs[i, 2] = precision(y_hat, y)
  }
  return(pairs)
}
predictions = matrix(nrow=50, ncol=1)
for (i in 1:50) {
  sample = testData[i, ]
  test = sample[idxs]
  p1 = 0
  p0 = 0
  idx = 1
  for (fidx in 1:10) {
    f_v = test[fidx]
    if (f_v == 0) {
      p0 = log2(p_cpt[idx, 2]) + p0
      p1 = log2(p_cpt[idx, 3]) + p1
    } else {
      p0 = log2(p_cpt[idx + 1, 2]) + p0
      p1 = log2(p_cpt[idx + 1, 3]) + p1
    }
    idx = idx + 2
  }
  #### normalize p1, p0
  sum_p = 2^(p1) + 2^(p0)
  p1 = 2^(p1) / sum_p
  predictions[i, 1] = p1
}
preds = t(predictions)
true = t(testData[, 1001])
roc = roc_helper(preds, true)
roc = roc[order(roc[, 1], decreasing=FALSE), ]
plot(roc[, 1], roc[, 2], type = "l", xlab = "FPR", ylab = "TPR", main="ROC Cruve")
pr = pr_helper(preds, true)
pr = pr[order(pr[, 1], decreasing=FALSE), ]
plot(pr[, 1], pr[, 2], type = "l", xlab = "recall", ylab = "precision", main="Precision Recall Curve")


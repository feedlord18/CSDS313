from random import random
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm, binom
import math

bern_total = []
norm_total = []

bern_mean = []
norm_mean = []

def gen_bino():
    return binom.rvs(n=10, p=0.3, size=100)

def gen_norm():
    return norm.rvs(loc=-1, scale=math.sqrt(2), size=100)

for bin in range(1000):
    bern_list = gen_bino()
    norm_list = gen_norm()
    # store all entries
    bern_total.extend(bern_list)
    norm_total.extend(norm_list)
    # now we calculate the mean
    bern_mean.append(bern_list.mean())
    norm_mean.append(norm_list.mean())

# calculate population mean and variance
mu = np.mean(bern_total)
var = np.var(bern_total)
sigma = math.sqrt(var)
print('Binomial\n\tMean:', mu, 'Variance:', var)
plt.figure(1)
# find x values
x = set()
for i in bern_mean:
    x.add(i)
x = list(x)
plt.hist(bern_mean)
plt.plot(x, norm.pdf(x, mu, sigma))
plt.title('binomial sample means')
plt.savefig("q2_bern.png")

mu = np.mean(norm_total)
var = np.var(norm_total)
sigma = math.sqrt(var)
print('Normal\n\tMean:', mu, 'Variance:', var)
plt.figure(2)
# find x values
x = set()
for i in norm_mean:
    x.add(i)
x = list(x)
plt.hist(norm_mean)
plt.plot(x, norm.pdf(x, mu, sigma))
plt.title('normal sample means')
plt.savefig("q2_norm.png")
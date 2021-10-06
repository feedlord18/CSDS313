from random import random
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
from scipy.stats import norm, binom
import math

binom_total = []
norm_total = []

binom_mean = []
norm_mean = []

def gen_bino():
    return binom.rvs(n=10, p=0.4, size=100)

def gen_norm():
    return norm.rvs(loc=-1, scale=math.sqrt(2), size=100)

for bin in range(1000):
    binom_list = gen_bino()
    norm_list = gen_norm()
    # store all entries
    binom_total.extend(binom_list)
    norm_total.extend(norm_list)
    # now we calculate the mean
    binom_mean.append(binom_list.mean())
    norm_mean.append(norm_list.mean())

mu = np.mean(binom_mean)
var = np.var(binom_mean)
sigma = math.sqrt(var)
print('Binomial\n\tMean:', mu, 'Standard Deviation:', sigma)
x = set()
for i in binom_mean:
    x.add(i)
print('binomial unique means:', len(x))
plt.figure(1)
plt.hist(binom_mean, len(x), density=True, alpha=0.6, color='b')
xmin, xmax = plt.xlim()
x = np.linspace(xmin, xmax, len(x))
plt.plot(x, norm.pdf(x, mu, sigma), 'k', linewidth=2)
plt.title('binomial sample means')
plt.savefig("q2_binom.png")

mu = np.mean(norm_mean)
var = np.var(norm_mean)
sigma = math.sqrt(var)
print('Normal\n\tMean:', mu, 'Standard Deviation:', sigma)
x = set()
for i in norm_mean:
    x.add(i)
print('normal unique means:', len(x))
plt.figure(2)
plt.hist(norm_mean, len(x), density=True, alpha=0.6, color='b')
xmin, xmax = plt.xlim()
x = np.linspace(xmin, xmax, len(x))
plt.plot(x, norm.pdf(x, mu, sigma), 'k', linewidth=2)
plt.title('normal sample means')
plt.savefig("q2_norm.png")
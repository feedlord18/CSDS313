from random import random
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

bern_total = []
norm_total = []

bern_mean = []
norm_mean = []

def gen_bernoulli():
    if random() < 0.3:
        return 1
    else:
        return 0

def gen_norm():
    return np.random.normal(-1, 2)

for bin in range(1000):
    bern_list = []
    norm_list = []
    for i in range(100):
        bern_list.append(gen_bernoulli())
        norm_list.append(gen_norm())
    # store all entries
    bern_total.append(bern_list)
    norm_total.append(norm_list)
    # now we calculate the mean
    bern_mean.append(np.sum(bern_list)/100)
    norm_mean.append(np.sum(norm_list)/100)

plt.figure(1)
plt.hist(bern_mean)
print(norm.pdf(bern_mean))
plt.plot(bern_mean, norm.pdf(bern_mean), label='norm pdf')
plt.title('bernoulli sample means')
plt.savefig("q2_bern.png")
plt.figure(2)
plt.hist(norm_mean)
plt.plot(norm_mean, norm.pdf(norm_mean), label='norm pdf')
plt.title('normal sample means')
plt.savefig("q2_norm.png")
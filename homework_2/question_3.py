from random import random
import numpy as np
from scipy.stats import ttest_ind_from_stats

filename = './data/cars.csv'

jap_car = []
us_car = []

def read_file():
    with open(filename, 'r') as f:
        file = f.readlines()

    for line in file:
        # US, JAP
        newline = line.strip('\n')
        tokens = newline.split(',')
        if tokens[0] is not '':
            us_car.append(int(tokens[0]))
        if tokens[1] is not '':
            jap_car.append(int(tokens[1]))

read_file()
print('japanese cars:', len(jap_car))
print('us cars:', len(us_car))
print('')

# we obtain samples statistics
jap_mu = np.mean(jap_car)
jap_std = np.std(jap_car)
us_mu = np.mean(us_car)
us_std = np.std(us_car)

print('Japanese\n\t', 'mean:', "{:.2f}".format(jap_mu), 'standard deviation:', "{:.2f}".format(jap_std))
print('US\n\t', 'mean:', "{:.2f}".format(us_mu), 'standard deviation:', "{:.2f}".format(us_std))
print('')
# perform testing with scipy library
t_stat, p_value = ttest_ind_from_stats(jap_mu, jap_std, len(jap_car), us_mu, us_std, len(us_car))
print('t-statistic:', "{:.3f}".format(t_stat), 'p-value:', "{:.3f}".format(p_value))
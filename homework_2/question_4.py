from random import random
import numpy as np
from scipy.stats import f_oneway

filename = './data/gears.csv'

batch_map = dict()

def read_file():
    with open(filename, 'r', encoding='utf-8-sig') as f:
        file = f.readlines()

    for line in file:
        # US, JAP
        newline = line.strip('\n')
        tokens = newline.split(',')
        v = float(tokens[0])
        batch = int(tokens[1])
        if batch not in batch_map:
            batch_map[batch] = []
        batch_map[batch].append(v)

read_file()

# now we just use scipy f_oneway and return ANOVA f-statistic and p-value
f_statistic, p_value = f_oneway(batch_map[1], batch_map[2], batch_map[3], 
                                    batch_map[4], batch_map[5], batch_map[6], 
                                    batch_map[7], batch_map[8], batch_map[9], batch_map[10])
print('f-statistic:', "{:.3f}".format(f_statistic), 'p-value:', "{:.3f}".format(p_value))
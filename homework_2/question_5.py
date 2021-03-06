import numpy as np
from scipy.stats import powerlaw, expon, uniform, norm
import matplotlib.pyplot as plt

route_list = []
rating_list = []

airport_file = './data/airport_routes.csv'
movie_file = './data/movie_votes.csv'

def read_file():

    # read airport file
    with open(airport_file, 'r', encoding='utf-8-sig') as f:
        file = f.readlines()

    i = 0
    for line in file:
        if i == 0:
            i = i + 1
            continue
        newline = line.strip('\n')
        tokens = newline.split(',')
        routes = int(tokens[-1])
        route_list.append(routes)
    
    f.close()
    
    # read movie file
    with open(movie_file, 'r', encoding='utf-8-sig') as f:
            file = f.readlines()

    i = 0
    for line in file:
        if i == 0:
            i = i + 1
            continue
        newline = line.strip('\n')
        tokens = newline.split(',')
        rating = float(tokens[-1])
        rating_list.append(rating)

    f.close()
    
def fit_power(data):
    plt.clf()
    bins = len(np.unique(data))
    plt.hist(data, bins, density=True, alpha=0.6, color='b')
    a, loc, scale = powerlaw.fit(data)
    x = np.linspace(np.unique(data)[0], np.unique(data)[-1], bins)
    fit_data = powerlaw.pdf(x, a=a)
    print('power alpha:', "{:.3f}".format(a))
    print('powerlaw samples:', powerlaw.rvs(a, loc=loc, scale=scale, size=10))
    plt.plot(x, fit_data, 'k')
    if len(data) == 3409:
        plt.savefig('./question_5/airport_power_fit.png')
    else:
        plt.savefig('./question_5/movie_power_fit.png')

def fit_exp(data):
    plt.clf()
    bins = len(np.unique(data))
    plt.hist(data, bins, density=True, alpha=0.6, color='b')
    loc, scale = expon.fit(data)
    x = np.linspace(np.unique(data)[0], np.unique(data)[-1], bins)
    fit_data = expon.pdf(x, loc, scale)
    print('exponential lambda:', "{:.3f}".format(1/scale))
    print('exponential samples:', expon.rvs(loc=loc, scale=scale, size=10))
    plt.plot(x, fit_data, 'k')
    if len(data) == 3409:
        plt.savefig('./question_5/airport_expon_fit.png')
    else:
        plt.savefig('./question_5/movie_expon_fit.png')

def fit_uni(data):
    plt.clf()
    bins = len(np.unique(data))
    plt.hist(data, bins, density=True, alpha=0.6, color='b')
    loc, scale = uniform.fit(data)
    x = np.linspace(np.unique(data)[0], np.unique(data)[-1], bins)
    fit_data = uniform.pdf(x, loc, scale)
    print('uniform a, b:', "{:.3f}".format(loc), "{:.3f}".format(loc + scale))
    print('uniform samples:', uniform.rvs(loc=loc, scale=scale, size=10))
    plt.plot(x, fit_data, 'k')
    if len(data) == 3409:
        plt.savefig('./question_5/airport_uni_fit.png')
    else:
        plt.savefig('./question_5/movie_uni_fit.png')

def fit_norm(data):
    plt.clf()
    bins = len(np.unique(data))
    plt.hist(data, bins, density=True, alpha=0.6, color='b')
    loc, scale = norm.fit(data)
    x = np.linspace(np.unique(data)[0], np.unique(data)[-1], bins)
    fit_data = norm.pdf(x, loc, scale)
    print('normal mean and std:', "{:.3f}".format(loc), "{:.3f}".format(scale))
    print('uniform samples:', norm.rvs(loc=loc, scale=scale, size=10))
    plt.plot(x, fit_data, 'k')
    if len(data) == 3409:
        plt.savefig('./question_5/airport_norm_fit.png')
    else:
        plt.savefig('./question_5/movie_norm_fit.png')

read_file()
# print some statistics
print(len(route_list), 'airports:\n\t', 'min conn:', np.min(route_list), 'max conn', np.max(route_list))
print(len(rating_list), 'movies:\n\t', 'min rate:', np.min(rating_list), 'max rate', np.max(rating_list))
print('')

# fit airport
print('---------------------------------')
print('airport distribution parameters:')
fit_power(route_list)
fit_exp(route_list)
fit_uni(route_list)
fit_norm(route_list)
print('---------------------------------')
print('movie distribution parameters:')
# fit movie
fit_power(rating_list)
fit_exp(rating_list)
fit_uni(rating_list)
fit_norm(rating_list)
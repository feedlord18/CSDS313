from random import random, seed


table = dict()

trials = [10, 100, 1000, 10000, 100000, 1000000]

def roll_dice():
    i = random()
    if i < 0.25:
        return 'A'
    elif i >= 0.75:
        return 'T'
    elif i < 0.75 and i >= 0.5:
        return 'C'
    else:
        return 'G'

def print_summary(n):
    print('MLE estimates:')
    print('A: occurs=' + str(handle('A')), 'p=' + str(0 if (handle('A') == 0) else handle('A')/n))
    print('T: occurs=' + str(handle('T')), 'p=' + str(0 if (handle('T') == 0) else handle('T')/n))
    print('C: occurs=' + str(handle('C')), 'p=' + str(0 if (handle('C') == 0) else handle('C')/n))
    print('G: occurs=' + str(handle('G')), 'p=' + str(0 if (handle('G') == 0) else handle('G')/n))
    print('---------------------------------------------')

def handle(letter):
    try:
        return table[letter]
    except KeyError:
        return 0

for n in trials:
    # we generate n samples from set (A, T, C, G)
    for i in range(n):
        temp = roll_dice()
        if temp not in table:
            table[temp] = 1
        else:
            table[temp] = table[temp] + 1
    print('printing summary for', n, 'samples.')
    print_summary(n)
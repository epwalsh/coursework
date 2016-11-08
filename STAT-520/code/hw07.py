#!/usr/bin/env python
# =============================================================================
# File Name:     hw07.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-04
# Last Modified: 2016-11-08 17:38:35
# =============================================================================

import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


MU = 0
TAU2 = 100
COV_SCALE = 10000
M = 11000
BURN_IN = 1000

d = pd.read_csv('../data/HW07.csv')
y = d['y'].values
x = d['x'].values


def inverse_information(betas):
    '''
    Compute the inverse fisher information at beta.
    '''
    b0 = betas[0]
    b1 = betas[1]
    l = np.exp(b0 + b1 * x)
    upper_left = np.sum(l)
    off_diag = np.sum(x * l)
    lower_right = np.sum(x * x * l)
    fish_inf = np.array([[upper_left, off_diag], [off_diag, lower_right]])
    return np.linalg.inv(fish_inf)


def rand_walk_candidates(betas, scale=COV_SCALE):
    '''
    Propose new values from a random walk candidate distribution.
    '''
    cov = scale * inverse_information(betas)
    return np.random.multivariate_normal(betas, cov)


def acceptance_prob_rwalk(b_prev, b_next):
    '''
    Compute the acceptance probability.
    '''
    log_num = np.sum(y * (b_next[0] + x * b_next[1]) - np.exp(b_next[0] + x * b_next[1]))
    log_num = log_num - (b_next[0] ** 2) / (2 * TAU2) - (b_next[1] ** 2) / (2 * TAU2)
    log_den = np.sum(y * (b_prev[0] + x * b_prev[1]) - np.exp(b_prev[0] + x * b_prev[1]))
    log_den = log_den - (b_prev[0] ** 2) / (2 * TAU2) - (b_prev[1] ** 2) / (2 * TAU2)
    res = np.exp(log_num - log_den)
    return min(res, 1)


if __name__ == '__main__':
    beta0 = np.array([.5], dtype=np.float64)
    beta1 = np.array([.5], dtype=np.float64)
    accept_or_not = []
    
    print 'Executing sampling process'
    
    for i in xrange(M):
        old_b = [beta0[i], beta1[i]]
        new_b = rand_walk_candidates(old_b)
        alpha = acceptance_prob_rwalk(old_b, new_b)
        accept = np.random.binomial(1, alpha)
        accept_or_not.append(accept)
        new_b = new_b if accept else old_b
        beta0 = np.append(beta0, new_b[0])
        beta1 = np.append(beta1, new_b[1])
    
        # Print progress
        prop_fin = 1.0 * i / M
        progress = '=' * int(67.0 * prop_fin)
        progress = progress + ' ' * (66 - len(progress))
        sys.stdout.write(progress + '| %2.0f%%\r' % (100 * prop_fin))
        sys.stdout.flush()
    
    
    beta0 = beta0[BURN_IN+1:]
    beta1 = beta1[BURN_IN+1:]
    
    print '\nAcceptance rate:', 1.0 * sum(accept_or_not[BURN_IN+1:]) / (M - BURN_IN)
    print '\n5 number summary for beta0:'
    print np.percentile(beta0, [0, 25, 50, 75, 100])
    print '5 number summary for beta1:'
    print np.percentile(beta1, [0, 25, 50, 75, 100])

    print '\nShowing trace plots...'
    sns.set_style("darkgrid")
    plt.plot(beta0)
    plt.show()
    plt.plot(beta1)
    plt.show()

#!/usr/bin/env python
# =============================================================================
# File Name:     hw07_gibbs.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-04
# Last Modified: 2016-11-09 11:55:01
# =============================================================================

import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


MU = 0
TAU2 = 100
COV = 0.01
M = 11000
BURN_IN = 1000

d = pd.read_csv('../data/HW07.csv')
y = d['y'].values
x = d['x'].values


def rand_walk_candidate(beta, v=COV):
    '''
    Propose new values from a random walk candidate distribution.
    '''
    return np.random.normal(beta, v)


def acceptance_prob_b0(b1, b0_p, b0_n):
    '''
    Compute the acceptance probability for beta0 candidates.
    '''
    log_num = np.sum(y * (b0_n + x * b1) - np.exp(b0_n + x * b1))
    log_num = log_num - (b0_n ** 2) / (2 * TAU2) 
    log_den = np.sum(y * (b0_p + x * b1) - np.exp(b0_p + x * b1))
    log_den = log_den - (b0_p ** 2) / (2 * TAU2)
    res = np.exp(log_num - log_den)
    return min(res, 1)


def acceptance_prob_b1(b0, b1_p, b1_n):
    '''
    Compute the acceptance probability for beta1 candidates.
    '''
    log_num = np.sum(y * (b0 + x * b1_n) - np.exp(b0 + x * b1_n))
    log_num = log_num - (b1_n ** 2) / (2 * TAU2) 
    log_den = np.sum(y * (b0 + x * b1_p) - np.exp(b0 + x * b1_p))
    log_den = log_den - (b1_p ** 2) / (2 * TAU2)
    res = np.exp(log_num - log_den)
    return min(res, 1)


if __name__ == '__main__':
    beta0 = np.array([0], dtype=np.float64)
    beta1 = np.array([.5], dtype=np.float64)
    b0_acceptance = []
    b1_acceptance = []
    
    print 'Executing sampling process'
    
    for i in xrange(1, M):
        # Simulate new beta0 conditioned on old beta1
        old_b0 = beta0[i-1]
        new_b0 = rand_walk_candidate(old_b0)
        alpha = acceptance_prob_b0(beta1[i-1], old_b0, new_b0)
        accept = np.random.binomial(1, alpha)
        new_b0 = new_b0 if accept else old_b0
        b0_acceptance.append(accept)

        # Simulate new beta1 conditioned on new beta0
        old_b1 = beta1[i-1]
        new_b1 = rand_walk_candidate(old_b1)
        alpha = acceptance_prob_b1(new_b0, old_b1, new_b1)
        accept = np.random.binomial(1, alpha)
        new_b1 = new_b1 if accept else old_b1
        b1_acceptance.append(accept)

        beta0 = np.append(beta0, new_b0)
        beta1 = np.append(beta1, new_b1)

        # Print progress
        prop_fin = 1.0 * i / M
        progress = '=' * int(67.0 * prop_fin)
        progress = progress + ' ' * (66 - len(progress))
        sys.stdout.write(progress + '| %2.0f%%\r' % (100 * prop_fin))
        sys.stdout.flush()
    
    
    beta0 = beta0[BURN_IN+1:]
    beta1 = beta1[BURN_IN+1:]
    
    print '\nAcceptance rate for beta0:', 1.0 * sum(b0_acceptance[BURN_IN:]) / (M - BURN_IN)
    print '\nAcceptance rate for beta1:', 1.0 * sum(b1_acceptance[BURN_IN:]) / (M - BURN_IN)
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

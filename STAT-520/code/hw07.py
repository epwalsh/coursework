#!/usr/bin/env python
# =============================================================================
# File Name:     hw07.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-04
# Last Modified: 2016-11-04 18:03:44
# =============================================================================

import sys

import numpy as np
import pandas as pd
from scipy.misc import factorial


MU = 0
TAU2 = 100
CANDIDATE_VAR = 100
M = 11000
BURN_IN = 1000


def density_y(y, x, beta0, beta1):
    '''
    Returns the joint density function of y given beta0 and beta1. y and x are 
    arrays, beta0 and beta1 are scalars.
    '''
    assert(x.shape == y.shape)
    l = np.exp(beta0 + beta1 * x)
    return np.prod((l ** y) * np.exp(-l) / factorial(y))


def dnorm(x, mu=0, v=1):
    '''
    Computes the density of a normal distribution at x.
    '''
    kernel = np.exp(-((x - mu)**2) / (2.0 * v))
    return ((2.0 * np.pi * v)**(-.5)) * kernel
    

def density_beta(beta):
    '''
    Returns the marginal prior of beta given the sclars mu and tau2.
    '''
    return dnorm(beta, MU, TAU2)


def posterior(y, x, beta0, beta1):
    '''
    Returns the density of the posterior.
    '''
    fy = density_y(y, x, beta0, beta1)
    pi0 = density_beta(beta0)
    pi1 = density_beta(beta1)
    return fy * pi0 * pi1


def rand_candidates(v=CANDIDATE_VAR):
    '''
    Returns a candidate value from a normal distribution with mean beta.
    '''
    return np.random.normal(np.sqrt(v), size=2)


def rand_walk_candidates(betas, v=CANDIDATE_VAR):
    cov = np.array([[v,0], [0,v]])
    return np.random.multivariate_normal(betas, cov)


def acceptance_prob(y, x, b_previous, b_next):
    '''
    Computes the acceptance probability.
    '''
    num = posterior(y, x, b_next[0], b_next[1]) * dnorm(b_previous[0], 0,
                                                        CANDIDATE_VAR)
    den = posterior(y, x, b_previous[0], b_previous[1]) * dnorm(b_next[0], 0,
                                                                CANDIDATE_VAR)
    if den == 0 or np.isnan(num) or np.isnan(den):
        return 1
    return min(num / den, 1)


def acceptance_prob_rwalk(y, x, b_previous, b_next):
    '''
    Computes the acceptance probability.
    '''
    num = posterior(y, x, b_next[0], b_next[1])
    den = posterior(y, x, b_previous[0], b_previous[1])
    if den == 0 or np.isnan(den):
        return 1
    if np.isnan(num):
        return 0
    return min(num / den, 1)


if __name__ == '__main__':
    d = pd.read_csv('../data/HW07.csv')
    y = d['y'].values
    x = d['x'].values
    
    beta0 = np.array([0], dtype=np.float64)
    beta1 = np.array([0], dtype=np.float64)
    
    print 'Executing sampling process'
    
    for i in xrange(M):
        old_b = [beta0[i], beta1[i]]
        #  new_b = rand_candidates()
        #  alpha = acceptance_prob(y, x, old_b, new_b)
        new_b = rand_walk_candidates(old_b)
        alpha = acceptance_prob_rwalk(y, x, old_b, new_b)
        accept = np.random.binomial(1, alpha)
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
    
    print '\n5 number summary for beta0:'
    print np.percentile(beta0, [0, 0.25, 0.5, 0.75, 1])
    print '5 number summary for beta1:'
    print np.percentile(beta1, [0, 0.25, 0.5, 0.75, 1])

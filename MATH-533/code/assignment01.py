#!/usr/bin/env python
# =============================================================================
# File Name:     assignment01.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2017-01-23
# Last Modified: 2017-01-23 21:12:16
# =============================================================================



ALPHABET = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']


def char_freq(m):
    n = 1.0 * len(m)
    return {k: (m.count(k) / n) for k in ALPHABET}


def ioc(m):
    n = 1.0 * len(m)
    return sum([(m.count(k) / n)**2 for k in ALPHABET])

m = "MSCUPTEHCIPHCJICPTEHCQBPUSPJKEOEHCKSCRNCQVOEHCQYXKDCKTEHCJLESXQQVLCEQLCPU"\
    "GFEICSEICFEICIEQCQPONGDVKSPUMNDJEUSPQGNULFEEDNGLCSEEBCQQEEJCBCDKKQXIPUCNJ"\
    "CUSEMCQHCUUCGVTESUJEUUNWEUREQGFPQKCQQCPLPUYXJSFDNUKEUYXJSEYPIP"

frequencies = char_freq(m)
for k in frequencies.keys():
    print k, frequencies[k]

ioc(m)


FRQ_ENGLISH = {'A':  8.167, 'B': 1.492, 'C': 2.782, 'D': 4.253, 'E': 12.702,
               'F': 2.228, 'G': 2.015, 'H': 6.094, 'I': 6.966, 'J': 0.153,
               'K': 0.772, 'L': 4.025, 'M': 2.406, 'N': 6.749, 'O': 7.507,
               'P': 1.929, 'Q': 0.095, 'R': 5.987, 'S': 6.327, 'T': 9.056,
               'U': 2.758, 'V': 0.978, 'W': 2.360, 'X': 0.150, 'Y': 1.974,
               'Z': 0.074}
IOC_ENGLISH = sum([(FRQ_ENGLISH[k] / 100)**2 for k in ALPHABET])
IOC_ENGLISH

import operator



def dgrams(m):
    dgrams = []
    for i in range(len(m)-1):
        dgrams.append(m[i:i+2])
    dgrams = set(dgrams)
    return {d: m.count(d) for d in dgrams}


def tgrams(m):
    dgrams = []
    for i in range(len(m)-2):
        dgrams.append(m[i:i+3])
    dgrams = set(dgrams)
    return {d: m.count(d) for d in dgrams}

digrams = dgrams(m)
trigrams = tgrams(m)
sorted(frequencies.items(), key=operator.itemgetter(1), reverse=True)
sorted(digrams.items(), key=operator.itemgetter(1), reverse=True)[1:30]
sorted(trigrams.items(), key=operator.itemgetter(1), reverse=True)[1:10]


def try_partial_key(m, key):
    return ''.join([key[c] if c in key.keys() else c for c in m])

key = {'E': 'T', 'I': 'H', 'C': 'E', 'Q': 'A'}
try_partial_key(m, key)
frequencies['N']

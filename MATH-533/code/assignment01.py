#!/usr/bin/env python
# =============================================================================
# File Name:     assignment01.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2017-01-23
# Last Modified: 2017-01-23 19:10:24
# =============================================================================



ALPHABET = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']


def char_freq(m):
    return {k: m.count(k) for k in ALPHABET}


m = 'BAAABLAH'
char_freq(m)

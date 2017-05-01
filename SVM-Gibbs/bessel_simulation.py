#!/usr/bin/env python
# =============================================================================
# File Name:     bessel_simulation.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2017-04-21
# Last Modified: 2017-05-01 10:47:59
# =============================================================================

"""
docstring
"""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

from scipy.special import kv

vals = range(1, 700)
for x in vals:
    (kv(0, x) / kv(0.5, x)) / (x**0.5)

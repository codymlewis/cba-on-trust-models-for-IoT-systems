#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Plot the distance equation, to see the effects of context setting attacks on
the returned values.

Author: Cody Lewis
Date: 2019-06-25
'''

import matplotlib.pyplot as plt
import numpy as np


def dsq(target, j):
    '''give the 1d distance squared between the target and j'''
    return np.abs(target - j)**2

def dist(St, C_vals, Sj, Cj, eta=1):
    '''Give the distance values'''
    Cmax = 101
    Smax = 101
    distances = []

    for Ct in C_vals:
        dSmaxsq = dsq(St, Smax)
        dCmaxsq = dsq(Ct, Cmax)
        distances.append(
            min(
                np.sqrt(
                    (dSmaxsq + dCmaxsq) *
                    ((dsq(St, Sj) / dSmaxsq) + (dsq(Ct, Cj) / dCmaxsq))
                ),
                np.sqrt(
                    (dSmaxsq + dCmaxsq) *
                    (
                        ((Cmax - Cj) / (Cmax - (Ct - eta)))**2 +
                        (Sj / (St + eta))**2
                    )
                )
            )
        )

    return np.array(distances)

if __name__ == '__main__':
    SERVICES = [1, 16, 33, 50, 66, 82, 100]
    C_vals = np.arange(1, 101)
    Cj = 50
    service_line = dict()
    for Sj in SERVICES:
        plt.plot(C_vals, dist(Sj, C_vals, Sj, Cj), label=f"Service {Sj}")

    plt.title("Ranges of Distance with Perfect service and Capability Setting")
    plt.xlabel("Capability Target")
    plt.ylabel("Distance")
    plt.legend()

    plt.show()

'''
Various functions used for the system
'''

import numpy as np

import json


def d(a, b):
    '''Get the Euclidean distance between a and b'''
    axis = 0
    if isinstance(b, np.ndarray):
        axis = len(b.shape) - 1
    return np.sqrt(np.abs(np.sum((a - b) * (b - a), axis=axis)))


def sigmoid_s(z):
    '''Shifted sigmoid'''
    return sigmoid(z - 1)


def sigmoid(z):
    '''Logistic sigmoid'''
    return 1 / (1 + np.exp(-z))


def load_params():
    '''Load the parameters from a json file.'''
    with open('params.json', 'r') as f:
        p = json.load(f)
    return p

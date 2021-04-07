import numpy as np

def d(a, b):
    axis = 0
    if isinstance(b, np.ndarray):
        axis = len(b.shape) - 1
    return np.sqrt(np.abs(np.sum((a - b) * (b - a), axis=axis)))

def sign(x):
    return f if (f := np.sign(x)) != 0 else 1

# rm
def s(a, b):
    "Get the similarity between a and b where they are in [-1, 1]"
    return int(sign(a) == sign(b))

def sigmoid_s(z):
    return sigmoid(z - 1)

def sigmoid(z):
    return 1 / (1 + np.exp(-z))

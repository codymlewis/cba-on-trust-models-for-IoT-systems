'''
The observer makes measurements of the system without interacting with it.
'''


import numpy as np

from node import Node


class Observer(Node):
    '''An observer node.'''
    def __init__(self, i, params):
        super().__init__(i, params)
        self.current_c = np.array(
            [0] + [0.5 for _ in range(params['number_contexts'] - 1)]
        )

    def increment(self):
        '''Only increment the time.'''
        self.current_c[0] += 1

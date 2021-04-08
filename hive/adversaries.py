'''
Defines the possible adversaries for the network.
'''

import numpy as np

from node import Node


class BadMouther(Node):
    '''A bad mouthing node'''
    def send_rec(self, node):
        '''Alway report the trust as -1'''
        node.get_rec(self.id, self.current_c, -1)


class ContextAttacker(Node):
    '''A context attacking node'''
    def __init__(self, i, params):
        super().__init__(i, params)
        self.attack_prev = False

    def send_rec(self, node):
        '''Sometimes perform the 2 stage context attack'''
        if self.attack_prev or np.random.uniform(0, 1) <= 0.8:
            self.attack_prev = not self.attack_prev
            node.get_rec(
                self.id,
                np.array(
                    [self.current_c[0] - (500 if self.attack_prev else 0)] +
                    [0.5 for _ in range(self.params['number_contexts'] - 1)]
                ),
                -1
            )
        else:
            super().send_rec(node)


def load_adversary(adv_type):
    '''Factory for adversaries'''
    return {
        "BadMouther": BadMouther,
        "ContextAttacker": ContextAttacker,
    }[adv_type]

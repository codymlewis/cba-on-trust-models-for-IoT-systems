import numpy as np

from Node import Node

class BadMouther(Node):
    def send_rec(self, node):
        node.get_rec(self.id, self.current_c, -1)


class ContextAttacker(Node):
    def __init__(self, i, params):
        super().__init__(i, params)
        self.attack_prev = False

    def send_rec(self, node):
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
    return {
        "BadMouther": BadMouther,
        "ContextAttacker": ContextAttacker,
    }[adv_type]

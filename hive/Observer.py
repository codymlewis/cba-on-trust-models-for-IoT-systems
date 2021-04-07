import numpy as np

from Node import Node


class Observer(Node):
    def __init__(self, i, params):
        super().__init__(i, params)
        self.current_c = np.array(
            [0] + [0.5 for _ in range(params['number_contexts'] - 1)]
        )

    def increment(self):
        self.current_c[0] += 1

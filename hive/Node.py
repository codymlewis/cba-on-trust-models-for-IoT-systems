from itertools import cycle

import numpy as np

from Functions import *


class Node:
    def __init__(self, i, params):
        self.id = i
        self.params = params
        self.R = np.array([params['init_trust'] for _ in range(params['number_nodes'])])
        self.R_cached = np.zeros(params['number_nodes'])
        self.QC = np.zeros(params['number_nodes'])
        self.c = np.zeros((params['number_nodes'], params['number_contexts']))
        self.use_c = np.zeros((params['number_nodes'], params['number_contexts']))
        self.cs = np.zeros((params['number_nodes'], params['zeta'], params['number_contexts']))
        self.c_i = [cycle(range(params['zeta'])) for _ in range(params['number_nodes'])]
        self.c_cached = np.zeros((params['number_nodes'], params['number_contexts']))
        self.C = np.zeros((params['number_nodes'], params['number_contexts']))
        self.C_cached = np.zeros((params['number_nodes'], params['number_contexts']))
        self.Ts = np.array([params['init_trust'] for _ in range(params['number_nodes'])])
        self.T = 0
        self.crecv = np.zeros((params['number_nodes'], params['number_contexts']))
        self.Tdir = params['init_trust']
        self.got_recs = []
        self.ever_got_recs = []
        self.current_c = np.concatenate(
            (
                np.array([0]),
                np.random.uniform(0, 1, self.params['number_contexts'] - 1)
            )
        )

    def update_rep(self, i):
        cached_comb = self.R_cached[i] * self.params['eta']**d(self.c[i], self.C_cached[i])
        idx = list(set(self.got_recs) - {i})
        self.R_cached[i] = self.R[i]
        self.R[i] = np.clip(cached_comb + self.params['delta_r'] *
                sum(self.Ts[i] * self.Ts[idx]), -1, 1)

    def update_QC(self, i):
        self.QC[i] = sigmoid_s(d(self.c[i], self.C[i]))

    def can_recommend(self, i):
        return self.Ts[i] >= self.params['trust_threshold'] or \
    self.params['eta']**(d(self.current_c, self.crecv[i]) + self.Ts[i] *
            sigmoid_s(d(self.c[i], self.c_cached[i]))) <= self.params['tau']

    def update_direct_trust(self, fb):
        self.Tdir = np.clip(self.Tdir + self.params['delta'] * fb, -1, 1)

    def calc_T(self):
        if self.ever_got_recs:
            idx = self.ever_got_recs
            dists = sigmoid_s(-d(self.current_c, self.use_c[idx]))
            indT = np.sum(self.QC[idx] * dists * self.R[idx] *
                    self.Ts[idx]) / np.sum(dists)
        else:
            indT = 0
        alpha = sigmoid(2 * np.abs(self.Tdir) / dist if (dist := d(self.Tdir,
            indT)) > 0 else 0)
        beta = 1 - alpha
        self.T = alpha * self.Tdir + beta * indT
        return self.T

    def update_C(self, i):
        self.C_cached[i] = self.C[i]
        cs = self.cs[i, 0:min(int(self.current_c[0]) + 1, self.params['zeta'] + 1)]
        if len(cs):
            dists = self.params['theta']**d(self.c[i], cs)
        else:
            dists = self.params['theta']**d(self.c[i], np.zeros(self.params['number_contexts']))
        self.C[i] = np.sum((dists * cs.T).T, axis=0) / np.sum(dists)

    def transact(self, nodes, sp):
        self.calc_T()
        fb = sp.serve()
        self.update_direct_trust(fb)
        self.update_on_recs()
        for i, node in enumerate(nodes):
            if i != self.id:
                self.send_rec(node)

    def update_on_recs(self):
        for i in self.got_recs:
            self.update_rep(i)
            self.update_QC(i)
            self.update_C(i)
        self.got_recs = []

    def send_rec(self, node):
        node.get_rec(self.id, self.current_c, self.Tdir)

    def get_rec(self, i, c, T):
        if self.can_recommend(i):
            self.c_cached[i] = self.c[i]
            self.use_c[i] = c
            self.cs[i, next(self.c_i[i])] = c
            self.Ts[i] = T
            self.crecv[i] = self.current_c
            self.got_recs.append(i)
            if i not in self.ever_got_recs:
                self.ever_got_recs.append(i)
        self.c[i] = c

    def increment(self):
        self.current_c[0] += 1
        self.current_c[1:] = np.clip(self.current_c[1:] + np.random.normal(0,
            0.01, self.params['number_contexts'] - 1), 0, 1)

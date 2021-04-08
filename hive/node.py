'''
A node within the system, calculated all trust model values.
'''


from itertools import cycle

import numpy as np

import functions as f


class Node:
    '''The TMS node.'''
    def __init__(self, i, params):
        self.id = i
        self.params = params
        self.R = np.array(
            [params['init_trust'] for _ in range(params['number_nodes'])]
        )
        self.R_cached = np.zeros(params['number_nodes'])
        self.QC = np.zeros(params['number_nodes'])
        self.c = np.zeros(
            (params['number_nodes'], params['number_contexts'])
        )
        self.use_c = np.zeros(
            (params['number_nodes'], params['number_contexts'])
        )
        self.cs = np.zeros(
            (params['number_nodes'], params['zeta'], params['number_contexts'])
        )
        self.c_i = [
            cycle(range(params['zeta'])) for _ in range(params['number_nodes'])
        ]
        self.c_cached = np.zeros(
            (params['number_nodes'], params['number_contexts'])
        )
        self.C = np.zeros((params['number_nodes'], params['number_contexts']))
        self.C_cached = np.zeros(
            (params['number_nodes'], params['number_contexts'])
        )
        self.Ts = np.array(
            [params['init_trust'] for _ in range(params['number_nodes'])]
        )
        self.T = 0
        self.crecv = np.zeros(
            (params['number_nodes'], params['number_contexts'])
        )
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
        '''Update the reputation.'''
        forget = self.params['eta']**f.d(self.c[i], self.C_cached[i])
        cached_comb = self.R_cached[i] * forget
        idx = list(set(self.got_recs) - {i})
        self.R_cached[i] = self.R[i]
        gradient = self.params['delta_r'] * sum(self.Ts[i] * self.Ts[idx])
        self.R[i] = np.clip(
            cached_comb + gradient,
            -1,
            1
        )

    def update_QC(self, i):
        '''Update the quality of context.'''
        self.QC[i] = f.sigmoid_s(f.d(self.c[i], self.C[i]))

    def can_recommend(self, i):
        '''See if node i should be allowed to recommend the sp.'''
        p = self.Ts[i] >= self.params['trust_threshold']
        dist = f.d(self.current_c, self.crecv[i])
        tscale = self.Ts[i] * f.sigmoid_s(f.d(self.c[i], self.c_cached[i]))
        q = self.params['eta']**(dist + tscale) <= self.params['tau']
        return p or q

    def update_direct_trust(self, fb):
        '''Update the direct trust.'''
        self.Tdir = np.clip(self.Tdir + self.params['delta'] * fb, -1, 1)

    def calc_T(self):
        '''Calculate the trust of sp.'''
        if self.ever_got_recs:
            idx = self.ever_got_recs
            dists = f.sigmoid_s(-f.d(self.current_c, self.use_c[idx]))
            ind_t = np.sum(
                self.QC[idx] * dists * self.R[idx] * self.Ts[idx]
            ) / np.sum(dists)
        else:
            ind_t = 0
        alpha = f.sigmoid(
            2 * np.abs(self.Tdir) / dist
            if (dist := f.d(self.Tdir, ind_t)) > 0 else 0
        )
        beta = 1 - alpha
        self.T = alpha * self.Tdir + beta * ind_t
        return self.T

    def update_C(self, i):
        '''Update the aggregated context.'''
        self.C_cached[i] = self.C[i]
        cs = self.cs[i, 0:min(int(self.current_c[0]) + 1, self.params['zeta'] + 1)]
        if len(cs):
            dists = self.params['theta']**f.d(self.c[i], cs)
        else:
            dists = self.params['theta']**f.d(
                self.c[i], np.zeros(self.params['number_contexts'])
            )
        self.C[i] = np.sum((dists * cs.T).T, axis=0) / np.sum(dists)

    def transact(self, nodes, sp):
        '''
        Calculate trust, perform a transaction, update values, send
        recommendations.
        '''
        self.calc_T()
        fb = sp.serve()
        self.update_direct_trust(fb)
        self.update_on_recs()
        for i, node in enumerate(nodes):
            if i != self.id:
                self.send_rec(node)

    def update_on_recs(self):
        '''
        Update repuation based values for nodes that sent a recommendation
        this epoch
        '''
        for i in self.got_recs:
            self.update_rep(i)
            self.update_QC(i)
            self.update_C(i)
        self.got_recs = []

    def send_rec(self, node):
        '''Send a recommendation to the node.'''
        node.get_rec(self.id, self.current_c, self.Tdir)

    def get_rec(self, i, c, T):
        '''Get a recommendation from node i.'''
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
        '''Increment the context.'''
        self.current_c[0] += 1
        self.current_c[1:] = np.clip(
            self.current_c[1:] +
            np.random.normal(
                0,
                0.01,
                self.params['number_contexts'] - 1
            ),
            0,
            1
        )

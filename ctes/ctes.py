from dataclasses import dataclass
import sys

import numpy as np

from tqdm import trange, tqdm


N_SERVERS = 1
N_SERVICES = 10


@dataclass
class Server:
    quality: float

    def serve(self):
        return np.random.rand() < self.quality


class Node:
    server: np.ndarray
    location: np.ndarray
    services_exp: np.ndarray
    alpha: np.ndarray
    beta: np.ndarray
    trust: np.ndarray

    def __init__(self, server: int, location: np.ndarray):
        self.server = np.array([1 if i == server else 0 for i in range(N_SERVERS)], dtype=float)
        self.location = np.array(location, dtype=float)
        self.services_exp = np.zeros(N_SERVICES, dtype=float)
        self.alpha = np.full((N_SERVERS, N_SERVICES), 1, dtype=float)
        self.beta = np.full((N_SERVERS, N_SERVICES), 1, dtype=float)
        self.trust = np.full((N_SERVERS, N_SERVICES), 0.5, dtype=float)
    
    def exp_service(self, service):
        self.services_exp[service] = 1

    def rate_service(self, server, service, good):
        if good:
            self.alpha[server, service] += 1
        else:
            self.beta[server, service] += 1
        self.trust[server, service] = self.alpha[server, service] / (self.alpha[server, service] + self.beta[server, service])

class Observer(Node):
    def __init__(self, server, location, service):
        super().__init__(server, location)
        self.server_id = server
        self.service = service


@dataclass
class Target:
    loc: np.ndarray
    server: int
    service: int


class Adversary(Node):
    def __init__(self, server, location, target):
        super().__init__(server, location)
        self.location = target.loc
        self.trust = np.full((N_SERVERS, N_SERVICES), 1, dtype=float)
        self.trust[target.server, target.service] = 0
        self.services_exp[target.service] = 1
        self.target = target
    
    def rate_service(self, server, service, good):
        if server != self.target.server or service != self.target.service:
            super().rate_service(server, service, good)
    


def sim(x, y):
    x += sys.float_info.epsilon
    y += sys.float_info.epsilon
    return np.dot(x, y) / (np.linalg.norm(x) * np.linalg.norm(y))



def direct_trust(alpha, beta):
    return alpha / (alpha + beta)


def recommended_trust(requestor, nodes, server, service):
    rec_sims = []
    rec_trusts = []
    for node in nodes:
        SIs = sim(node.server, requestor.server)
        SIl = sim(node.location, requestor.location)
        SIp = node.services_exp[service]
        if min(SIs, SIl, SIp) >= 0.5:
            rec_sims.append(SIs + SIl + SIp)
            rec_trusts.append(node.trust[server, service])
    if len(rec_sims) == 0:
        return np.array(0.0)
    rec_sims, rec_trusts = np.array(rec_sims), np.array(rec_trusts)
    return ((rec_sims / rec_sims.sum()) * rec_trusts).sum()
    

def weigh_param(experience, direct_trust, rec_trust, eps=0.001):
    exp_dir_dis = abs(experience - direct_trust)
    if exp_dir_dis < eps:
        return np.random.uniform(0.8, 1)
    if exp_dir_dis < eps and direct_trust != rec_trust:
        return np.random.uniform(0, 0.2)
    exp_rec_dis = abs(experience - rec_trust)
    if exp_dir_dis < eps and exp_rec_dis < eps and max(direct_trust, rec_trust) < eps:
        return np.random.uniform(0.41, 0.6)
    if exp_dir_dis < exp_rec_dis:
        return np.random.uniform(0.61, 0.8)
    return np.random.uniform(0.21, 0.4)


def total_trust(experience, direct_trust, rec_trust):
    mu = weigh_param(experience, direct_trust, rec_trust)
    return mu * direct_trust + (1 - mu) * rec_trust


if __name__ == "__main__":
    total_nodes = 100
    total_epochs = 500
    adversaries = [0.0, 0.2, 0.5, 0.8, 1.0]
    results = [[] for _ in adversaries]
    target = Target(np.array([50, 50], dtype=float), 0, 1)
    for a, adv in enumerate(adversaries):
        tqdm.write(f"Running with {adv:.0%} adversaries")
        servers = [Server(1.0) for _ in range(N_SERVERS)]
        nodes = [Node(0, np.random.randint(1, 101, size=2)) for _ in range(int((1 - adv) * total_nodes))]
        nodes += [Adversary(0, np.random.randint(1, 101, size=2), target) for _ in range(int(adv * total_nodes))]
        observer = Observer(0, target.loc, target.service)
        pbar = trange(total_epochs)
        for epoch in pbar:
            for i, node in enumerate(nodes):
                chosen_server = 0
                chosen_service = np.random.randint(0, N_SERVICES)
                rec = recommended_trust(node, nodes[:i] + nodes[i + 1:], chosen_server, chosen_service)
                if epoch == 0 or total_trust(servers[chosen_server].quality, node.trust[chosen_server, chosen_service], rec) >= 0.5:
                    node.exp_service(chosen_service)
                    node.rate_service(chosen_server, chosen_service, servers[chosen_server].serve())
            rec = recommended_trust(observer, nodes, observer.server_id, observer.service)
            trust = total_trust(servers[observer.server_id].quality, observer.trust[observer.server_id, observer.service], rec)
            pbar.set_postfix({'Trust': f"{trust:.3f}", 'rec': f"{rec:.3f}"})
            results[a].append(trust)
    with open((fn := "results.csv"), 'w') as f:
        f.write(f"time,{','.join(['T' + str(a) for a in adversaries])}\n")
        for i in range(total_epochs):
            f.write(f"{i},{','.join([str(r[i]) for r in results])}\n")
    print(f"Written results to {fn}")

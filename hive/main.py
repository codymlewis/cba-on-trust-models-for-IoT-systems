#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from Params import load_params
from Node import Node
from ServiceProvider import ServiceProvider
from Adversaries import load_adversary
from Observer import Observer


if __name__ == '__main__':
    params = load_params()
    T = [[] for _ in params['percent_adv']]
    for p, p_adv in enumerate(params['percent_adv']):
        print(f"Running sim with {p_adv:%} adversaries...")
        nodes = []
        for i in range(params['number_nodes']):
            if i < p_adv * params['number_nodes']:
                nodes.append(load_adversary(params['adv_type'])(i, params))
            else:
                nodes.append(Node(i, params))
        observer = Observer(params['number_nodes'] + 1, params)
        nodes.append(observer)
        sp = ServiceProvider()
        for e in range(params['epochs']):
            for n in nodes[:-1]:
                n.transact(nodes, sp)
            T[p].append(observer.calc_T())
            print(f"\rEpoch {e + 1}/{params['epochs']} T = {T[p][-1]}", end=" ")
            observer.update_on_recs()
            for n in nodes:
                n.increment()
        print("Done.")
    print(f"Writing results to {params['results']}")
    with open(params['results'], 'w') as f:
        f.write(f"t,{','.join([str(p * 100) for p in params['percent_adv']])}\n")
        for i in range(len(T[0])):
            f.write(f"{i},{','.join([str(t[i]) for t in T])}\n")
    print("Done.")

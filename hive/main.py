#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from Params import load_params
from Node import Node
from ServiceProvider import ServiceProvider
from Adversaries import load_adversary
from Observer import Observer


if __name__ == '__main__':
    params = load_params()
    nodes = []
    for i in range(params['number_nodes']):
        if i < params['percent_adv'] * params['number_nodes']:
            nodes.append(load_adversary(params['adv_type'])(i, params))
        else:
            nodes.append(Node(i, params))
    observer = Observer(params['number_nodes'] + 1, params)
    nodes.append(observer)
    sp = ServiceProvider()
    T = []
    for e in range(params['epochs']):
        for n in nodes[:-1]:
            n.transact(nodes, sp)
        T.append(observer.calc_T())
        print(f"\rEpoch {e + 1}/{params['epochs']} T = {T[-1]}",
                end=" ")
        observer.update_on_recs()
        for n in nodes:
            n.increment()
    with open(params['results'], 'w') as f:
        f.write("t,T\n")
        for i, t in enumerate(T):
            f.write(f"{i},{t}\n")
    print("Done.")

def load_params():
    return {
            "number_nodes": 10,
            "percent_adv": 0.5,
            "adv_type": "ContextAttacker",
            "init_trust": 0.01,
            "number_contexts": 4,
            "eta": 0.95,
            "delta": 0.01,
            "delta_r": 0.02,
            "tau": 1,
            "theta": 0.7,
            "zeta": 50,
            "trust_threshold": 0.0,
            "epochs": 300,
            "results": "results.csv"
    }

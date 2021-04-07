import json

def load_params():
    with open('params.json', 'r') as f:
        p = json.load(f)
    return p

# HIVE
A trust management system that mitigates context-based attacks, along with
normal note-based attacks.

## Pre-requisites
For the python requirements run in shell
```
pip install -r requirements.txt
```

For the R plotting requirements, run in R
```
install.packages(c('reshape', 'ggplot2', 'rjson'))
```

## Running
The main simulation is run using `./main.py` and is configured with
`params.json`. You can plot the results with `plot.R`.

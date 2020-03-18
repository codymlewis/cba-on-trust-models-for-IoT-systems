# Li 19 Trust Model

A simulator of the trust model proposed in Context-Aware Trust Management
System for IoT Applications with Multiple Domains

![Gif of map](https://gitlab.com/codymlewis/li-19-trust-model/raw/master/map.gif)


## Requirements

- R
- make

## Set-up

```
make deps
make all
```

## Configuration

The parameters may be configured either by altering the json file in `inst/extdata/`
or by specifying a different json file.

## Running

For a single command line simulation run:
```
./Simulation.R
```

For a single graphical simulation run:
```
./GUI.R
```

For a batch of simulations across differing numbers of adversaries and types run:
```
./BatchSimulation.R
```

## Pre-push hooks

```
ln config/pre-push.sh .git/hooks/pre-push
```

## API Reference

see https://codymlewis.gitlab.io/li-19-trust-model/li19trustmodel.pdf

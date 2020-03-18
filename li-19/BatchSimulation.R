#!/usr/bin/env Rscript

library(li19trustmodel)

main <- function() {
    batch_simulation(500, config="inst/extdata/params.json")
    quit("no")
}

main()

#!/usr/bin/env Rscript

library(li19trustmodel)

main <- function() {
    batch_simulation(500, config="inst/extdata/params.json", num_adversaries=c(8), adversary_types = c("ContextSetter"))
    quit("no")
}

main()

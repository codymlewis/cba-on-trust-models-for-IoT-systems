#!/usr/bin/env Rscript

library(li19trustmodel)

main <- function() {
    batch_simulation(500, config="inst/extdata/params.json", adversary_types = c("BadMouther"))
    quit("no")
}

main()

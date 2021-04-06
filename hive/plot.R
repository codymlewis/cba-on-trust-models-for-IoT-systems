#!/usr/bin/Rscript

library(ggplot2)


main <- function() {
    data <- read.csv('results.csv')
    ggplot(data=data, aes(x=t, y=T)) + geom_line() + scale_y_continuous(limits=c(-1, 1))
    ggsave("plot.png", width=7, height=7, dpi=320, type="cairo")
    cat("Saved plot.png\n")
}

main()

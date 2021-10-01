#!/usr/bin/Rscript

library(reshape)
library(ggplot2)


main <- function() {
    data <- read.csv('results.csv')
    names(data)[2:6] <- c("0%", "20%", "50%", "80%", "100% Adversaries")
    data <- melt(data, id=c("time"))
    ggplot(data=data, aes(x=time, y=value)) +
        geom_line(aes(color=variable)) +
        scale_y_continuous(limits=c(0, 1)) +
        ggplot2::labs(x = "Time", y = "Total trust", colour = NULL, shape = NULL) +
        ggplot2::theme(legend.position = "bottom")
    ggsave("plot.png", width=7, height=7, dpi=320, type="cairo")
    cat("Saved plot.png\n")
}

main()
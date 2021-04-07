#!/usr/bin/Rscript

library(reshape)
library(ggplot2)
library(rjson)


main <- function() {
    cat("Creating and writing plot...\n")
    params <- fromJSON(file='params.json')
    data <- melt(read.csv(params$results), id=c("t"))
    levels(data$variable) <- paste(
        substr(levels(data$variable), 2, nchar(levels(data$variable)) - 2),
        "%",
        sep=''
    )
    levels(data$variable)[length(levels(data$variable))] <-
        paste(levels(data$variable)[length(levels(data$variable))], "Adversaries")
    plt <- ggplot(data=data, aes(x=t, y=value)) +
            geom_line(aes(color=variable)) +
            labs(
                x = "Epochs",
                y = "Trust value",
                colour = NULL,
                shape = NULL
            ) +
            scale_y_continuous(limits=c(-1, 1)) +
            theme(legend.position="bottom")
    fn <- sprintf("hive_%s.png", params$adv_type)
    ggsave(fn, width=7, height=7, dpi=320, type="cairo")
    cat(sprintf("Saved %s\n", fn))
}

main()

#!/usr/bin/env Rscript

sum.con <- function(x, c, i, t.i)
{
    l <- x[[i]]
    m <- t.i ** abs(l - c[[i - 1]])
    return ((l + m*c[[i - 1]]) / (1 + m))
}


all.con <- function(x, c, i, t.i)
{
    l <- x[[i]]
    m <- t.i ** abs(l - x[1:(i - 1)])
    return ((l + sum(m * x[1:(i - 1)])) / (1 + sum(m)))
}


main <- function()
{
    theta.i <- 0.8
    x <- runif(1000)
    # x <- 1:1000
    for (theta.i in seq(0, 1, 0.05)) {
        c <- 0
        xlab <- "Number Summaries"
        ylab <- "Weighted-Average Context"
        for (i in 2:length(x)) {
            c[[i]] <- sum.con(x, c, i, theta.i)
        }
        ylim <- c(0, `if`(log(max(c), base=10) <= 0, 1, max(c)))
        filename <- sprintf("img/sum_context_%f.png", theta.i)
        png(filename)
        plot(c, ylim=ylim, main="Summarised context summarization", xlab=xlab, ylab=ylab)
        dev.off()
        cat(sprintf("Wrote %s\n", filename))

        for (i in 2:length(x)) {
            c[[i]] <- all.con(x, c, i, theta.i)
        }
        filename <- sprintf("img/all_context_%f.png", theta.i)
        png(filename)
        plot(c, ylim=ylim, main="All context summarization", xlab=xlab, ylab=ylab)
        dev.off()
        cat(sprintf("Wrote %s\n", filename))
    }
}


main()

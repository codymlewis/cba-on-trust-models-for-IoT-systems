H <- function(x) {
    return(-sum(x * log(x, base = 3)))
}


find_trust <- function(x) {
    return(0.3 * (1 - H(x)))
}


find_new_rep <- function(o, nr, omega = 0.981) {
    return(omega*o + omega*0.1**(omega*abs(o))*nr)
}


rep_fnr <- function(o, nr, reps, omega = 0.981) {
    for (i in 1:reps) {
        o <- find_new_rep(o, nr, omega)
    }
    return(o)
}


plot_reachable_trust <- function(om = 0.95, t = 0:79) {
    y <- sapply(
        t,
        function(i) {
            rep_fnr(
                0.01,
                find_trust(
                    c(
                        (i + 1) / (i + 3),
                        1 / (i + 3),
                        1 / (i + 3)
                    )
                ),
                200,
                om = om
            )
        }
    )
    y[1] <- 0.01
    Cairo::CairoPNG(
        filename=sprintf("omega=%f.png", om),
        height = 1080 / 2,
        width = 1920 / 2
    )
    plot(
        c(0, t + 1),
        c(0, y * (2 - 0.5**0.3)),
        type = "o",
        xlab = "Number of Good Contacts",
        ylab = "Reachable Trust",
        col = "blue"
    )
    axis(1, at=seq(0, 80, by=5))
    dev.off()
}

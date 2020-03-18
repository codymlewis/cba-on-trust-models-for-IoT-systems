#!/usr/bin/env Rscript


# Create a progress bar
cat_progress <- function(current, total, progress_len = 31, prefix = "", postfix = "") {
    progress <- floor(100 * current / total)
    progress_bar_progress <- floor(progress_len * progress * 0.01)

    if (progress_bar_progress != 0) {
        unprogressed <- progress_len - progress_bar_progress
    } else {
        unprogressed <- progress_len - 1
    }

    progress_bar <- "["
    progress_bar <- paste(
        c(
            progress_bar,
            `if`(
                progress_bar_progress - 2 > 0,
                rep("█", progress_bar_progress - 2),
                ""
            )
        ),
        collapse = ""
    )

    progress_bar <- paste(c(progress_bar, rep(" ", unprogressed), "]"), collapse = "")
    progress_percent <- paste(c(progress, "%"), collapse = "")
    postfix <- paste(c(postfix, `if`(progress == 100, "\n", "")), collapse = "")

    cat(sprintf("\r%s %s %s %s", prefix, progress_bar, progress_percent, postfix))
}

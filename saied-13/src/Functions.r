#!/usr/bin/env Rscript

# Create a progress bar
cat_progress <- function(current, total, progress_len=31, prefix="") {
    progress = floor(100 * current / total)
    progress_bar_progress = floor(progress_len * progress * 0.01)
    if(progress_bar_progress != 0) {
        unprogressed = progress_len - progress_bar_progress
    } else {
        unprogressed = progress_len - 1
    }
    progress_bar = "["
    progress_bar = paste(
        c(
          progress_bar,
          `if`(progress_bar_progress - 2 > 0,
               rep("=", progress_bar_progress - 2), ""),
          `if`(unprogressed > 0, ">", "=")
          ),
        collapse=""
    )
    progress_bar = paste(c(progress_bar, rep(".", unprogressed), "]"), collapse="")
    progress_percent = paste(c(progress, "%", `if`(progress == 100, "\n", "")), collapse="")
    cat(sprintf("\r%s %s %s", prefix, progress_bar, progress_percent), end="\r")
}

# Create a progress bar
cat.progress <- function(current, total, progress_len=31, prefix="") {
    progress = floor(100 * current / total)
    progress_bar_progress = floor(progress_len * progress * 0.01)
    if(progress_bar_progress != 0) {
        unprogressed = progress_len - progress_bar_progress
    } else {
        unprogressed = progress_len - 1
    }
    progress_bar = "["
    progress_bar = paste(
        c(
          progress_bar,
          `if`(progress_bar_progress - 2 > 0,
               rep("=", progress_bar_progress - 2), ""),
          `if`(unprogressed > 0, ">", "=")
          ),
        collapse=""
    )
    progress_bar = paste(c(progress_bar, rep(".", unprogressed), "]"), collapse="")
    progress_percent = paste(c(progress, "%", `if`(progress == 100, "\n", "")), collapse="")
    cat(sprintf("\r%s %s %s", prefix, progress_bar, progress_percent), end="\r")
}

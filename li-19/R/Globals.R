#' @include Params.R
NULL

utils::globalVariables(
        c("LAND", "WATER", "AIR", "TRUSTED", "UNKNOWN", "DISTRUST", "params")
)

LAND <- 0
WATER <- 1
AIR <- 2

TRUSTED <- 0
UNKNOWN <- 1
DISTRUST <- 2

params <- Params$new()

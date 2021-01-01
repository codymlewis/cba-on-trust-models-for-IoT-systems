#' @include Functions.R
#' @include Tile.R

Field <- R6::R6Class(
        "Field",
        list(
                tiles = list(),

                initialize = function(data = NULL, verbose = F) {
                        base_stations <- place_base_stations(params$map_width, params$map_height)
                        self$tiles <- list()
                        if (verbose) {
                                cat("Creating field...\n")
                        }
                        self$add_tiles(data, base_stations, verbose)
                        grid_connect(self, base_stations)
                        invisible(self)
                },

                add_tiles = function(data, base_stations, verbose) {
                        "Add the tiles of the map"
                        for (i in 1:params$map_width) {
                                self$tiles[[i]] <- list()
                                for (j in 1:params$map_height) {
                                        self$tiles[[i]][[j]] <<- Tile$new(
                                                `if`(
                                                        !all(is.null(data)),
                                                        data[[i]][[j]],
                                                        `if`(round(runif(1)), 1, 0)
                                                )
                                        )
                                        self$add_base_stations(base_stations, i, j)
                                }
                                if (verbose) {
                                        cat_progress(
                                                i,
                                                params$map_width,
                                                prefix = sprintf("Column %d of %d", i, params$map_width)
                                        )
                                }
                        }
                        invisible(self)
                },

                add_base_stations = function(base_stations, i, j) {
                        "Possibly add base stations or base station signals to the current tile"
                        for (base_station in base_stations) {
                                if (euc_dist(base_station$location, c(i, j)) <= params$signal_radius) {
                                        is_edge <- (euc_dist(base_station$location, c(i + 1, j)) >
                                                params$signal_radius) ||
                                                (euc_dist(base_station$location, c(i, j + 1)) >
                                                        params$signal_radius) ||
                                                (euc_dist(base_station$location, c(i - 1, j)) >
                                                        params$signal_radius) ||
                                                (euc_dist(base_station$location, c(i, j - 1)) >
                                                        params$signal_radius)
                                        self$tiles[[i]][[j]]$add_signal(
                                                base_station,
                                                is_edge
                                        )
                                }
                                if (all(base_station$location == c(i, j))) {
                                        self$tiles[[i]][[j]]$add_base_station(base_station)
                                }
                        }
                        invisible(self)
                },

                size = function() {
                        "Get the size of the field"
                        return(length(self$tiles))
                },

                shape = function() {
                        "Get the shape of the field"
                        return(c(length(self$tiles[[1]]), length(self$tiles)))
                },

                get_tile = function(location) {
                        "Get the tile at the location if there is one, otherwise NA"
                        if (all(location <= self$shape()) && all(location > c(0, 0))) {
                                return(list(self$tiles[[location[[1]]]][[location[[2]]]]))
                        }
                        return(list())
                },

                add_service_provider = function(sp) {
                        cur_tile <- self$get_tile(sp$location)
                        if (length(cur_tile)) {
                                cur_tile[[1]]$add_service_provider(sp)
                        }
                        invisible(self)
                }
        )
)


# Place the base stations on a rectangle such that the signals cover the
# entirety of the rectangle
place_base_stations <- function(width, height) {
        gap <- compute_gap(params$signal_radius)
        base_stations <- list()

        for (i in seq(min(width / 2, gap), width, gap)) {
                for (j in seq(min(height / 2, gap), height, gap)) {
                        base_stations[[length(base_stations) + 1]] <- BaseStation$new(i, j)
                }
        }

        return(base_stations)
}


grid_connect <- function(field, base_stations) {
        gap <- compute_gap(params$signal_radius)
        for (base_station in base_stations) {
                cur_loc <- base_station$location
                other_tile <- field$get_tile(cur_loc - c(gap, 0))
                if (length(other_tile)) {
                        other_station <- other_tile[[1]]$get_base_station()
                        check_and_add_neighbour(base_station, other_station)
                }
                other_tile <- field$get_tile(cur_loc - c(0, gap))
                if (length(other_tile)) {
                        other_station <- other_tile[[1]]$get_base_station()
                        check_and_add_neighbour(base_station, other_station)
                }
        }
}


check_and_add_neighbour <- function(base_station, other_station) {
        base_station$add_neighbour(other_station)
}

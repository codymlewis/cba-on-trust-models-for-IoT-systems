Tile <- R6::R6Class(
        "Tile",
        list(
                objects = list(),
                obj_ids = NULL,
                signals = list(),
                terrain = NULL,
                base_station = list(),
                signal_edge = FALSE,
                service_provider = list(),

                initialize = function(terrain) {
                        self$terrain <- terrain
                        invisible(self)
                },

                add_device = function(device) {
                        "Add a device here"
                        self$objects[[device$id]] <- device
                        self$obj_ids <- c(self$obj_ids, device$id)
                        invisible(self)
                },

                add_base_station = function(base_station) {
                        "Add the base station here"
                        self$base_station[[1]] <- base_station
                        invisible(self)
                },

                add_service_provider = function(service_provider) {
                        self$service_provider[[1]] <- service_provider
                        invisible(self)
                },

                get_base_station = function() {
                        "Get the base station from here"
                        return(self$base_station[[1]])
                },

                rm_device = function(id) {
                        "Remove a device from here"
                        self$objects[[id]] <- 0
                        self$obj_ids <- self$obj_ids[self$obj_ids != id]
                        invisible(self)
                },

                has_devices = function() {
                        "TRUE if there are devices on this tile, else FALSE"
                        if (!is.null(self$obj_ids)) {
                                for (obj_id in self$obj_ids) {
                                        if (!is.numeric(self$objects[obj_id]) &&
                                                !is.null(self$objects[obj_id])) {
                                                return(TRUE)
                                        }
                                }
                        }
                        return(FALSE)
                },

                get_first_dev = function() {
                        return(self$objects[[self$obj_ids[[1]]]])
                },

                add_signal = function(base_station, is_edge) {
                        "Add a signal from a base station here"
                        self$signals[[length(self$signals) + 1]] <- base_station
                        if (is_edge) {
                                self$signal_edge <- TRUE
                        }
                        invisible(self)
                }
        )
)

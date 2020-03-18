Observer <- R6::R6Class(
    "Observer",
    inherit = Device,

    public = list(
        initialize = function(id, sp, map,
                              loc = round(
                                  runif(2, min = 1, max = c(params$map_width, params$map_height))
                              )) {
            super$initialize(
                id, sp, map, `if`(params$observer_targeted, params$target_location, loc)
            )
            if (params$observer_targeted) {
                self$capability <- params$target_capability
                self$velocity <- params$target_velocity
            }
            invisible(self)
        },

        transaction = function(devices, can_transact = TRUE) {
            super$transaction(devices, can_transact)
            self$acceptable_recs[[self$id]][[params$time_now]] <- FALSE
        },

        use_trust = function(normalized_c_target) {
            rs_dir_trust <- self$find_direct_trust(normalized_c_target)
            return(self$find_indirect_trust(normalized_c_target))
        },

        move = function() {
            if (params$observer_targeted) {
                self$time_last_moved <- params$time_now
            } else {
                super$move()
            }
            invisible(self)
        }
    )
)

BadMouther <- R6::R6Class(
    "BadMouther",
    inherit = Device,
    public = list(
        create_rec = function(rs_dir_trust) {
            return(
                Observation$new(
                    self$contexts[[self$id]][get_context_index(params$time_now)],
                    -1,
                    self$id
                )
            )
        },

        fill_estimated_trust = function(used_trust) {
            self$estimated_trusts[[params$time_now]] <- -1
            invisible(self)
        }
    )
)


ContextSetter <- R6::R6Class(
    "ContextSetter",
    inherit = Device,
    public = list(
        create_rec = function(rs_dir_trust) {
            return(
                Observation$new(
                    normalize(
                        c(
                            params$time_now,
                            params$target_capability,
                            euc_dist(
                                params$target_location,
                                self$service_provider$location
                            ),
                            params$target_velocity
                        )
                    ),
                    -1,
                    self$id
                )
            )
        },

        fill_estimated_trust = function(used_trust) {
            self$estimated_trusts[[params$time_now]] <- -1
            invisible(self)
        }
    )
)

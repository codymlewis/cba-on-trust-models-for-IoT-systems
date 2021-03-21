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
                attack_prev = FALSE,

                create_rec = function(rs_dir_trust) {
                        prob <- params$number_adversaries / (params$number_nodes - 1)
                        if (self$attack_prev || runif(1) < `if`(prob <= 0.5, 1.5, 1) * prob) {
                                self$attack_prev = !self$attack_prev
                                return(
                                        Observation$new(
                                                normalize(
                                                        c(
                                                                params$time_now - `if`(self$attack_prev, 0, 500),
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
                        } else {
                                return(super$create_rec(rs_dir_trust))
                        }
                },

                fill_estimated_trust = function(used_trust) {
                        self$estimated_trusts[[params$time_now]] <- -1
                        invisible(self)
                }
        )
)

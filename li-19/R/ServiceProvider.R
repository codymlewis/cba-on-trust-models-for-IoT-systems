ServiceProvider <- R6::R6Class(
        "ServiceProvider",
        list(
                id = NULL,
                location = NULL,
                transaction_results = NULL,

                initialize = function(id = 1, p_trust = 1, p_unknown = 0, p_distrust = 0) {
                        self$id <- id
                        self$location <- round(
                                runif(2, min = 1, max = c(params$map_width, params$map_height))
                        )
                        p_vals <- c(p_trust, p_unknown, p_distrust)
                        sample_factor <- 10**(
                                1 - floor(log(min(p_vals[p_vals != 0]), base = 10))
                        )
                        self$transaction_results <- c(
                                rep(TRUSTED, p_trust * sample_factor),
                                rep(UNKNOWN, p_unknown * sample_factor),
                                rep(DISTRUST, p_distrust * sample_factor)
                        )
                        invisible(self)
                },

                provide_service = function() {
                        return(sample(self$transaction_results, 1))
                }
        )
)

Observation <- R6::R6Class(
    "Observation",
    list(
        context = NULL,
        trust = NULL,
        id_sender = NULL,
        acceptable = NULL,

        initialize = function(context, trust, id_sender, acceptable = TRUE) {
            self$context <- context
            self$trust <- trust
            self$id_sender <- id_sender
            self$acceptable <- acceptable
            invisible(self)
        }
    )
)

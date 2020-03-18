BaseStation <- R6::R6Class(
    "BaseStation",
    list(
        location = NULL,
        table = NULL,
        neighbours = list(),
        updated = FALSE,

        initialize = function(x = 1, y = 1) {
            self$location <- c(x, y)
            self$table <- list(
                next_hop = c(list(), rep(NULL, params$number_nodes)),
                hops = rep(Inf, params$number_nodes)
            )
            invisible(self)
        },

        add_neighbour = function(base_station) {
            "Symmetrically add a new neighbouring base station to this"
            self$new_neighbour(base_station)
            base_station$new_neighbour(self)
            invisible(self)
        },

        new_neighbour = function(neighbour) {
            "Add a new neighbour to this"
            self$neighbours[[length(self$neighbours) + 1]] <- neighbour
            invisible(self)
        },

        connect = function(device) {
            "Connect to a device"
            self$table$next_hop[[device$id]] <- device
            self$table$hops[[device$id]] <- 0
            invisible(self)
        },

        disconnect = function(device) {
            "Disconnect from a device"
            self$table$next_hop[[device$id]] <- 0
            self$table$hops[[device$id]] <- Inf
            invisible(self)
        },

        retabulate = function(device) {
            "Recalculate the routing tables of each of the neighbours"
            self$updated <- TRUE
            for (neighbour in self$neighbours) {
                neighbour$tabulate_device(device, self, self$table$hops[[device$id]] + 1)
            }
            invisible(self)
        },

        tabulate_device = function(device, prev_base_station, hops) {
            "Update the routing table on the given device"
            if (!self$updated) {
                self$table$next_hop[[device$id]] <- prev_base_station
                self$table$hops[[device$id]] <- hops
                self$updated <- TRUE
                for (neighbour in self$neighbours) {
                    neighbour$tabulate_device(device, self, hops + 1)
                }
            }
            invisible(self)
        },

        finish_update = function() {
            "Finish with updating the routing tables"
            if (self$updated) {
                self$updated <- FALSE
                for (neighbour in self$neighbours) {
                    neighbour$finish_update()
                }
            }
            invisible(self)
        },

        find_device = function(dev_id) {
            "Route for the device with the given id"
            cur_device <- self
            while (cur_device$table$hops[[dev_id]] > 0) {
                cur_device <- cur_device$table$next_hop[[dev_id]]
            }
            return(cur_device$table$next_hop[[dev_id]])
        }
    )
)

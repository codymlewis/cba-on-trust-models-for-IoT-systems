Device <- R6::R6Class(
        "Device",
        list(
                id = NULL,
                contacts = NULL,
                location = NULL,
                current_goal = NULL,
                capability = NULL,
                velocity = NULL,
                trust = NULL,
                distrust = NULL,
                unknown = NULL,
                sp_trust = NULL,
                sp_distrust = NULL,
                sp_unknown = NULL,
                domain = NULL,
                reputations = NULL,
                service_provider = NULL,
                time_last_moved = NULL,
                estimated_trusts = NULL,
                map = list(),
                contexts = list(),
                stored_trusts = list(),
                cached_contexts = list(),
                observed_trusts = numeric(),
                acceptable_recs = list(),
                old_trusts = list(),
                old_contexts = list(),
                transacted = FALSE,

                initialize = function(id, sp, map = NULL, loc = NULL, copy = NULL) {
                        if (is.null(copy)) {
                                self$service_provider <- sp
                                self$setup_map(map, loc)
                                self$id <- id
                                self$set_trusts()
                                self$setup_signals(map)
                                self$velocity <- runif(1, min = 0, max = params$max_velocity)
                                self$capability <- runif(1, min = 1, max = params$max_capability)
                                self$reputations <- rep(params$init_reputation, params$number_nodes)
                                self$time_last_moved <- params$time_now - 1
                                self$estimated_trusts <- c(params$trust_new_contact)
                                self$setup_report_storage()
                                self$setup_reputations()
                        } else {
                                self$copy(copy)
                        }
                        invisible(self)
                },

                copy = function(cp) {
                        self$id <- cp$id
                        self$contacts <- cp$contacts
                        self$location <- cp$location
                        self$current_goal <- cp$current_goal
                        self$capability <- cp$capability
                        self$velocity <- cp$velocity
                        self$trust <- cp$trust
                        self$distrust <- cp$distrust
                        self$unknown <- cp$unknown
                        self$sp_trust <- cp$sp_trust
                        self$sp_distrust <- cp$sp_distrust
                        self$sp_unknown <- cp$sp_unknown
                        self$domain <- cp$domain
                        self$reputations <- cp$reputations
                        self$service_provider <- cp$service_provider
                        self$time_last_moved <- cp$time_last_moved
                        self$estimated_trusts <- cp$estimated_trusts
                        self$map <- cp$map
                        self$contexts <- cp$contexts
                        self$stored_trusts <- cp$stored_trusts
                        self$cached_contexts <- cp$cached_contexts
                        self$observed_trusts <- cp$observed_trusts
                        self$acceptable_recs <- cp$acceptable_recs
                        self$old_trusts <- cp$old_trusts
                        self$old_contexts <- cp$old_contexts
                        self$transacted <- cp$transacted
                        invisible(self)
                },

                setup_map = function(map, loc) {
                        "Setup the map realted values for this node"
                        if (!is.null(map)) {
                                self$map <- list(map)
                                self$location <- `if`(
                                        is.null(loc),
                                        round(runif(2, min = 1, max = map$shape())),
                                        loc
                                )
                                if (round(runif(1)) == 1) {
                                        self$domain <- AIR
                                } else {
                                        self$domain <- map$get_tile(self$location)[[1]]$terrain
                                }
                                self$new_goal()
                        } else {
                                self$location <- round(
                                        runif(
                                                2,
                                                min = 1,
                                                max = c(params$map_width, params$map_height)
                                        )
                                )
                                self$map <- list()
                                self$domain <- sample(c(AIR, LAND, WATER), 1)
                        }
                        invisible(self)
                },

                setup_signals = function(map) {
                        "Add the signal from this node to the field"
                        if (!is.null(map)) {
                                map$get_tile(self$location)[[1]]$add_device(self)
                                for (signal in map$get_tile(self$location)[[1]]$signals) {
                                        signal$connect(self)
                                }
                        }
                        invisible(self)
                },

                setup_report_storage = function() {
                        "Setup the report storing values for each node"
                        self$contexts <- lapply(
                                1:params$number_nodes,
                                function(i) {
                                        `if`(
                                                i == self$id,
                                                normalize(
                                                        c(
                                                                params$time_now,
                                                                self$capability,
                                                                euc_dist(self$location, self$service_provider$location),
                                                                self$velocity
                                                        )
                                                ),
                                                c(params$time_now, 0, 0, 0)
                                        )
                                }
                        )
                        self$acceptable_recs <- lapply(
                                1:params$number_nodes,
                                function(i) {
                                        c(FALSE)
                                }
                        )
                        self$stored_trusts <- lapply(
                                1:params$number_nodes,
                                function(i) {
                                        params$trust_new_contact
                                }
                        )
                        invisible(self)
                },

                setup_reputations = function() {
                        "Setup the reputation related values for each node"
                        self$reputations <- sapply(
                                1:params$number_nodes,
                                function(i) {
                                        `if`(
                                                i == self$id,
                                                params$rep_self,
                                                params$init_reputation
                                        )
                                }
                        )
                        self$cached_contexts <- lapply(
                                1:params$number_nodes,
                                function(i) {
                                        c(params$time_now - 1, 0, 0, 0)
                                }
                        )
                        self$old_contexts <- lapply(
                                1:params$number_nodes,
                                function(i) {
                                        c(params$time_now - 1, 0, 0, 0)
                                }
                        )
                        invisible(self)
                },

                add_contact = function(adds, devs) {
                        "Add the contacts specified in the list"
                        for (i in adds) {
                                if (length(self$contacts) >= params$max_number_contacts) {
                                        break
                                }
                                if (length(devs[[i]]$contacts) < params$max_number_contacts) {
                                        self$new_contact(i)
                                        devs[[i]]$new_contact(self$id)
                                }
                        }
                        invisible(self)
                },

                new_contact = function(add) {
                        "Add a single new contact"
                        self$contacts <- sort(union(self$contacts, add))
                        invisible(self)
                },

                set_trusts = function() {
                        "Set up the trusts for the service providers in the network"
                        self$trust <- rep(0, params$number_nodes)
                        self$distrust <- rep(0, params$number_nodes)
                        self$unknown <- rep(0, params$number_nodes)
                        self$sp_trust <- 0
                        self$sp_distrust <- 0
                        self$sp_unknown <- 0
                        invisible(self)
                },

                new_goal = function() {
                        "Find a new location to head towards"
                        while (all(self$current_goal == self$location) ||
                                (self$domain == WATER &&
                                        self$map[[1]]$get_tile(self$current_goal)[[1]]$terrain != WATER)) {
                                self$current_goal <- round(runif(2, min = 1, max = self$map[[1]]$shape()))
                        }
                        invisible(self)
                },

                sp_trust_increment = function() {
                        "Increment the trust count of the service provider"
                        self$sp_trust <- self$sp_trust + params$tdu_increment
                        invisible(self)
                },

                sp_distrust_increment = function() {
                        "Increment the distrust count of the service provider"
                        self$sp_distrust <- self$sp_distrust + params$tdu_increment
                        invisible(self)
                },

                sp_unknown_increment = function() {
                        "Increment the unknown count of the service provider"
                        self$sp_unknown <- self$sp_unknown + params$tdu_increment
                        invisible(self)
                },

                trust_increment = function(id_sender) {
                        "Increment the trust count of the sender"
                        self$trust[[id_sender]] <- self$trust[[id_sender]] + 1
                        invisible(self)
                },

                unknown_increment = function(id_sender) {
                        "Increment the unknown count of the sender"
                        self$unknown[[id_sender]] <- self$unknown[[id_sender]] + 1
                        invisible(self)
                },

                distrust_increment = function(id_sender) {
                        "Increment the distrust count of the sender"
                        self$distrust[[id_sender]] <- self$distrust[[id_sender]] + 1
                        invisible(self)
                },

                send_rec = function(devices) {
                        "Send a recommendation to the devices"
                        rs_dir_trust <- self$find_direct_trust(
                                self$contexts[[self$id]][get_context_index(params$time_now)],
                                recommendation = TRUE
                        )
                        self$stored_trusts[[self$id]][[params$time_now]] <- rs_dir_trust$trust_comb
                        self$emit_observation(
                                self$create_rec(rs_dir_trust),
                                devices
                        )
                        invisible(self)
                },

                create_rec = function(rs_dir_trust) {
                        "Create a recommendation"
                        return(
                                Observation$new(
                                        self$contexts[[self$id]][get_context_index(params$time_now)],
                                        rs_dir_trust$trust_comb,
                                        self$id,
                                        self$acceptable_recs[[self$id]][[params$time_now]]
                                )
                        )
                },

                recieve_observation = function(obs) {
                        "Receive a recommendation from the sender"
                        if (length(self$stored_trusts[[obs$id_sender]]) >= params$compression_factor) {
                                w_context <- find_weighted_context(
                                        c(self$contexts[[obs$id_sender]], obs$context)
                                )
                                self$stored_trusts[[obs$id_sender]] <- `if`(
                                        obs$id_sender == self$id,
                                        self$stored_trusts[[obs$id_sender]] <- direct_trust(
                                                c(self$stored_trusts[[obs$id_sender]], obs$trust),
                                                c(self$contexts[[obs$id_sender]], obs$context),
                                                w_context
                                        ),
                                        self$stored_trusts[[obs$id_sender]] <- indirect_trust(
                                                c(self$stored_trusts[[obs$id_sender]], obs$trust),
                                                self$reputations[[obs$id_sender]],
                                                c(self$contexts[[obs$id_sender]], obs$context),
                                                find_weighted_context(self$contexts[[self$id]]),
                                                w_context
                                        )
                                )
                                self$contexts[[obs$id_sender]] <- w_context
                        } else {
                                cw_len <- length(params$context_weights)
                                i <- `if`(
                                        params$compression_factor < Inf,
                                        length(self$stored_trusts[[obs$id_sender]]) + 1,
                                        params$time_now
                                )
                                self$contexts[[obs$id_sender]][
                                        get_context_index(i)
                                ] <- obs$context
                                self$stored_trusts[[obs$id_sender]][[i]] <- obs$trust
                        }
                        self$calc_acceptability(obs)
                        invisible(self)
                },

                calc_acceptability = function(obs) {
                        if (any(self$acceptable_recs[[obs$id_sender]])) {
                                if (self$acceptable_recs[[obs$id_sender]][[params$time_now - 1]]) {
                                        self$old_trusts[[obs$id_sender]] <-
                                                self$stored_trusts[[obs$id_sender]][[params$time_now - 1]]
                                        self$old_contexts[[obs$id_sender]] <-
                                                self$contexts[[obs$id_sender]][get_context_index(params$time_now - 1)]
                                }
                                if (self$old_trusts[[obs$id_sender]] <
                                        (params$delta_a - params$trust_rep_adj_range)) {
                                        self$acceptable_recs[[obs$id_sender]][[params$time_now]] <-
                                                obs$acceptable && (
                                                        obs$trust > (params$delta_a - params$trust_rep_adj_range) ||
                                                                acceptable_rec(
                                                                        find_weighted_context(
                                                                                c(
                                                                                        self$cached_contexts[[obs$id_sender]],
                                                                                        obs$context
                                                                                )
                                                                        ),
                                                                        self$old_contexts[[obs$id_sender]],
                                                                        self$old_trusts[[obs$id_sender]]
                                                                )
                                                )
                                } else {
                                        self$acceptable_recs[[obs$id_sender]][[params$time_now]] <- obs$acceptable
                                }
                                # if (obs$id_sender %in% c(1, 2)) {
                                        # print(obs)
                                        # print(self$acceptable_recs[[obs$id_sender]][[params$time_now]])
# cw = find_weighted_context(
#                                                                                 c(
#                                                                                         self$cached_contexts[[obs$id_sender]],
#                                                                                         obs$context
#                                                                                 )
#                                                                         )
# print(context_distance(cw, self$old_contexts[[obs$id_sender]]))
# print(self$old_trusts[[obs$id_sender]])
# print(self$old_contexts[[obs$id_sender]])
# print(cw)
# print("calc")
# print(abs(self$old_trusts[[obs$id_sender]] * params$eta[1]**(context_distance(cw, self$old_contexts[[obs$id_sender]]) / params$delta)))
#                                         print(
#                                                                 acceptable_rec(
#                                                                         find_weighted_context(
#                                                                                 c(
#                                                                                         self$cached_contexts[[obs$id_sender]],
#                                                                                         obs$context
#                                                                                 )
#                                                                         ),
#                                                                         self$old_contexts[[obs$id_sender]],
#                                                                         self$old_trusts[[obs$id_sender]]
#                                                                 )

#                                         )
#                                 }
                        } else {
                                self$acceptable_recs[[obs$id_sender]][[params$time_now]] <- obs$acceptable
                        }
                        invisible(self)
                },

                move = function() {
                        "Move towards the current goal"
                        time_change <- params$time_now - self$time_last_moved
                        old_signals <- self$get_signals()
                        self$disconnect_all()
                        movement_amount <- round(self$velocity * time_change)
                        movement <- `if`(movement_amount > 0, 1:movement_amount, NULL)
                        for (m in movement) {
                                best_weight <- Inf
                                best_loc <- NA
                                best_tile <- NA
                                for (i in (self$location[[1]] - 1):(self$location[[1]] + 1)) {
                                        for (j in (self$location[[2]] - 1):(self$location[[2]] + 1)) {
                                                loc <- c(i, j)
                                                tile <- self$map[[1]]$get_tile(loc)
                                                if (!all(loc == self$location) && length(tile)) {
                                                        tile <- tile[[1]]
                                                        cost <- `if`(
                                                                self$domain == AIR,
                                                                1,
                                                                `if`(
                                                                        self$domain == tile$terrain,
                                                                        1,
                                                                        2
                                                                )
                                                        )
                                                        weight <- cost + euc_dist(loc, self$current_goal)
                                                        if (weight < best_weight) {
                                                                best_weight <- weight
                                                                best_loc <- loc
                                                                best_tile <- tile
                                                        }
                                                }
                                        }
                                }
                                if (!all(is.na(best_loc))) {
                                        self$map[[1]]$get_tile(self$location)[[1]]$rm_device(self$id)
                                        self$location <- best_loc
                                        best_tile$add_device(self)
                                }
                        }
                        self$connect_all()
                        self$retabulate_all(old_signals)
                        self$velocity <- min(max(0, self$velocity + rnorm(1)), params$max_velocity)
                        if (all(self$location == self$current_goal)) {
                                self$new_goal()
                        }
                        self$time_last_moved <- params$time_now
                        invisible(self)
                },

                disconnect_all = function() {
                        "Disconnect from all base stations that this is currently connected to"
                        for (signal in self$map[[1]]$get_tile(self$location)[[1]]$signals) {
                                signal$disconnect(self)
                        }
                        invisible(self)
                },

                connect_all = function() {
                        "Connect to all base stations currently in range of this"
                        for (signal in self$map[[1]]$get_tile(self$location)[[1]]$signals) {
                                signal$connect(self)
                        }
                        invisible(self)
                },

                retabulate_all = function(old_signals) {
                        "After changing from being in one set of signals to another, make
            them recalculate their routing tables"
                        if (self$has_signal()) {
                                check_signals <- self$get_signals()
                        } else {
                                check_signals <- old_signals
                        }
                        for (signal in check_signals) {
                                signal$retabulate(self)
                        }
                        for (signal in check_signals) {
                                signal$finish_update()
                        }
                        invisible(self)
                },

                has_signal = function() {
                        "Check whether this has signal"
                        return(length(self$map[[1]]$get_tile(self$location)[[1]]$signals) > 0)
                },

                get_signals = function() {
                        "Get the list of signals in range of this"
                        return(self$map[[1]]$get_tile(self$location)[[1]]$signals)
                },

                transaction = function(devices, can_transact = TRUE) {
                        "Perform a transaction with a service provider"
                        normalized_c_target <- normalize(self$get_target_context())
                        used_trust <- self$use_trust(normalized_c_target)
                        self$acceptable_recs[[self$id]][[params$time_now]] <- FALSE
                        if (can_transact) {
                                if (used_trust > params$trust_rep_threshold - params$trust_rep_adj_range) {
                                        t_rs <- self$service_provider$provide_service()
                                        if (t_rs == TRUSTED) {
                                                self$sp_trust_increment()
                                        } else if (t_rs == UNKNOWN) {
                                                self$sp_unknown_increment()
                                        } else {
                                                self$sp_distrust_increment()
                                        }
                                        self$acceptable_recs[[self$id]][[params$time_now]] <- TRUE
                                }
                        }
                        self$fill_estimated_trust(used_trust)
                        self$contexts[[self$id]][get_context_index(params$time_now)] <- normalized_c_target
                        self$observed_trusts[[params$time_now]] <- weighted_trust(
                                compute_trust(self$sp_trust, self$sp_distrust, self$sp_unknown),
                                self$sp_trust,
                                self$sp_distrust,
                                self$sp_unknown
                        )
                        invisible(self)
                },

                use_trust = function(normalized_c_target) {
                        "Calculate the trust value to use"
                        rs_dir_trust <- self$find_direct_trust(normalized_c_target)
                        return(
                                `if`(
                                        abs(rs_dir_trust$trust_comb) <=
                                                (params$trust_rep_threshold + params$trust_rep_adj_range),
                                        self$find_indirect_trust(normalized_c_target),
                                        rs_dir_trust$trust_est
                                )
                        )
                },

                get_target_context = function() {
                        "Get the current target context"
                        if (params$rand_context) {
                                return(
                                        c(
                                                params$time_now,
                                                runif(1, min = 0, max = params$max_capability),
                                                euc_dist(
                                                        round(runif(2, min = 1, max = self$map[[1]]$size())),
                                                        self$service_provider$location
                                                ),
                                                runif(1, min = 0, max = params$max_velocity)
                                        )
                                )
                        }
                        return(
                                c(
                                        params$time_now,
                                        self$capability,
                                        euc_dist(self$location, self$service_provider$location),
                                        self$velocity
                                )
                        )
                },

                find_direct_trust = function(normalized_c_target, recommendation = FALSE) {
                        "Find the direct trust of the service provider"
                        trust_evaled <- `if`(
                                recommendation,
                                0,
                                weighted_trust(
                                        compute_trust(self$sp_trust, self$sp_distrust, self$sp_unknown),
                                        self$sp_trust,
                                        self$sp_distrust,
                                        self$sp_unknown
                                )
                        )
                        valid_trusts <- !is.na(self$observed_trusts)
                        valid_contexts <- !is.na(self$contexts[[self$id]])
                        context_weighted <- find_weighted_context(self$contexts[[self$id]][valid_contexts])
                        dir_trust <- direct_trust(
                                c(self$observed_trusts[valid_trusts], trust_evaled),
                                c(self$contexts[[self$id]][valid_contexts], context_weighted),
                                context_weighted
                        )
                        self$stored_trusts[[self$id]][[params$time_now]] <- dir_trust
                        return(
                                list(
                                        trust_est = estimate_trust(
                                                normalized_c_target,
                                                context_weighted,
                                                dir_trust
                                        ),
                                        # trust_comb = dir_trust,
                                        trust_comb = minimax(dir_trust, -1, 1),
                                        context_weighted = context_weighted
                                )
                        )
                },

                find_indirect_trust = function(normalized_c_target) {
                        "Find the indirect trust of the service provider"
                        considerations <- self$get_considerations()
                        ac <- unlist(self$get_all_contexts(considerations))
                        if (is.null(ac) || length(ac[ac >= 0]) == 0) {
                                return(params$trust_new_contact)
                        }
                        context_weighted <- find_weighted_context(ac)
                        ind_trust <- self$find_ind(context_weighted, considerations)
                        return(
                                estimate_trust(
                                        normalized_c_target,
                                        context_weighted,
                                        ind_trust
                                )
                        )
                },

                fill_estimated_trust = function(used_trust) {
                        self$estimated_trusts[[params$time_now]] <- used_trust
                        invisible(self)
                },

                get_considerations = function(excludes = c()) {
                        "Find which recommendations should be considered"
                        return(
                                lapply(
                                        1:params$number_nodes,
                                        function(i) {
                                                if (i %in% self$contacts & !i %in% excludes) {
                                                        acc_recs <- which(
                                                                self$acceptable_recs[[i]]
                                                        )
                                                        return(
                                                                `if`(
                                                                        length(acc_recs) > 0,
                                                                        tail(acc_recs, 1),
                                                                        0
                                                                )
                                                        )
                                                } else {
                                                        return(0)
                                                }
                                        }
                                )
                        )
                },

                get_all_contexts = function(considerations) {
                        "Get all of the context values that should be considered"
                        return(
                                lapply(
                                        self$contacts,
                                        function(i) {
                                                return(
                                                        `if`(
                                                                considerations[[i]] == 0,
                                                                NULL,
                                                                self$contexts[[i]][get_context_index(considerations[[i]])]
                                                        )
                                                )
                                        }
                                )
                        )
                },

                find_ind = function(context_weighted, considerations) {
                        "Calculate the indirect trust"
                        ow <- lapply(
                                self$contacts,
                                function(i) {
                                        # print(sprintf("Contexts[%d] = %f", i, self$contexts[[i]][get_context_index(considerations[[i]])]))
                                        return(
                                                `if`(
                                                        considerations[[i]] == 0,
                                                        NULL,
                                                        omega(
                                                                context_weighted,
                                                                self$contexts[[i]][
                                                                        get_context_index(considerations[[i]])
                                                                ]
                                                        )
                                                )
                                        )
                                }
                        )
                        denominator <- sum(unlist(ow))
                        numerator <- sum(
                                unlist(
                                        lapply(
                                                self$contacts,
                                                function(i) {
                                                        return(
                                                                `if`(
                                                                        considerations[[i]] == 0,
                                                                        NULL,
                                                                        ow[[which(i == self$contacts)]] * (
                                                                                omega(
                                                                                        self$cached_contexts[[i]],
                                                                                        self$contexts[[i]][
                                                                                                get_context_index(considerations[[i]])
                                                                                        ]
                                                                                ) *
                                                                                        self$reputations[[i]] *
                                                                                        self$stored_trusts[[i]][considerations[[i]]]
                                                                        )
                                                                )
                                                        )
                                                }
                                        )
                                )
                        )
                        return(numerator / denominator)
                },


                performance_updates = function() {
                        lapply(
                                self$contacts,
                                function(i) {
                                        self$performance_update(i)
                                }
                        )
                        invisible(self)
                },

                performance_update = function(id_sender) {
                        "Update the stored performance of the observer"
                        if (length(which(self$acceptable_recs[[id_sender]])) >= 1) {
                                prev_time <- tail(which(self$acceptable_recs[[id_sender]]), 2)[[1]]
                                if (all(c(self$acceptable_recs[[self$id]][c(prev_time, params$time_now)]))) {
                                        context_trust_now <- self$get_contexts_trust(self$id, params$time_now)
                                        context_trust_prev <- self$get_contexts_trust(self$id, prev_time)
                                        self$update_rep_tdu(
                                                id_sender,
                                                prev_time,
                                                context_trust_now,
                                                context_trust_prev
                                        )
                                } else {
                                        lapply(
                                                setdiff(self$contacts, id_sender),
                                                function(i) {
                                                        if (all(c(self$acceptable_recs[[i]][c(prev_time, params$time_now)]))) {
                                                                context_trust_now <- self$get_contexts_trust(i, params$time_now)
                                                                context_trust_prev <- self$get_contexts_trust(i, prev_time)
                                                                self$update_rep_tdu(
                                                                        id_sender,
                                                                        prev_time,
                                                                        context_trust_now,
                                                                        context_trust_prev
                                                                )
                                                        }
                                                }
                                        )
                                }
                        }
                        invisible(self)
                },

                get_contexts_trust = function(id, time) {
                        "Get the context and trust of node id from the time"
                        return(
                                list(
                                        context = self$contexts[[id]][
                                                get_context_index(time)
                                        ],
                                        trust = self$stored_trusts[[id]][[time]]
                                )
                        )
                },

                update_rep_tdu = function(id_sender, prev_time,
                                          context_trust_now, context_trust_prev) {
                        if (length(context_trust_now) > 0 && length(context_trust_prev) > 0) {
                                direct_trend <- trend_of_trust(
                                        `if`(prev_time == params$time_now, 0, context_trust_prev$trust),
                                        context_trust_now$trust,
                                        context_trust_prev$context,
                                        context_trust_now$context
                                )
                                indirect_trend <- trend_of_trust(
                                        `if`(
                                                prev_time == params$time_now,
                                                0,
                                                self$stored_trusts[[id_sender]][[prev_time]]
                                        ),
                                        self$stored_trusts[[id_sender]][[params$time_now]],
                                        self$contexts[[id_sender]][get_context_index(prev_time)],
                                        self$contexts[[id_sender]][get_context_index(params$time_now)]
                                )
                                trends_diff <- abs(direct_trend - indirect_trend)
                                trends_max <- max(abs(direct_trend), abs(indirect_trend))
                                if (trends_diff < trends_max) {
                                        self$trust_increment(id_sender)
                                } else if (trends_diff <= max(trends_max, params$trend_threshold)) {
                                        self$unknown_increment(id_sender)
                                } else {
                                        self$distrust_increment(id_sender)
                                }
                        }
                },

                combine_reps = function() {
                        lapply(
                                self$contacts,
                                function(i) {
                                        self$combine_rep(i)
                                }
                        )
                        invisible(self)
                },

                combine_rep = function(id_sender) {
                        "Find the new reputation for sender of recommendation"
                        if (self$acceptable_recs[[id_sender]][[params$time_now]]) {
                                sender_context <- self$contexts[[id_sender]][get_context_index(params$time_now)]
                                c_new <- find_weighted_context(
                                        c(self$cached_contexts[[id_sender]], sender_context)
                                )
                                self$reputations[[id_sender]] <- minimax(
                                        reputation_combination(
                                                self$old_contexts[[id_sender]],
                                                sender_context,
                                                c_new,
                                                self$reputations[[id_sender]],
                                                weighted_trust(
                                                        compute_trust(
                                                                self$trust[[id_sender]],
                                                                self$distrust[[id_sender]],
                                                                self$unknown[[id_sender]]
                                                        ),
                                                        self$trust[[id_sender]],
                                                        self$distrust[[id_sender]],
                                                        self$unknown[[id_sender]]
                                                )
                                        ),
                                        -1,
                                        1
                                )
                                # if (id_sender %in% c(1, 2)) {
                                #         print(sprintf("reputation[%d] = %f", id_sender, self$reputations[[id_sender]]))
                                # }
                                if (abs(self$reputations[[id_sender]]) <=
                                        params$trust_rep_adj_range) {
                                        self$reputations[[id_sender]] <- params$init_reputation
                                }
                                self$cached_contexts[[id_sender]] <- c_new
                        }
                        invisible(self)
                },

                emit_observation = function(observation, devices) {
                        "send observation to all contacts"
                        lapply(
                                self$contacts,
                                function(contact) {
                                        connection_data <- self$communicate(contact)
                                        if (connection_data[[1]] < Inf) {
                                                # routed communication
                                                connection_data[[2]]$recieve_observation(observation)
                                        } else if (euc_dist(
                                                devices[[contact]]$location,
                                                self$location
                                        ) <= params$dev_signal_radius) {
                                                # direct communication
                                                devices[[contact]]$recieve_observation(observation)
                                        }
                                }
                        )
                        invisible(self)
                },

                communicate = function(contact_id) {
                        "Communicate with a random contact"
                        this_tile <- self$map[[1]]$get_tile(self$location)[[1]]
                        best_signal <- 1
                        for (i in seq_len(length(this_tile$signals))) {
                                if (this_tile$signals[[i]]$table$hops[[contact_id]] <=
                                        this_tile$signals[[best_signal]]$table$hops[[contact_id]]) {
                                        best_signal <- i
                                }
                        }
                        if (this_tile$signals[[best_signal]]$table$hops[[contact_id]] < Inf) {
                                other_device <- this_tile$signals[[best_signal]]$find_device(contact_id)
                        } else {
                                other_device <- NULL
                        }
                        return(
                                list(
                                        this_tile$signals[[best_signal]]$table$hops[[contact_id]],
                                        other_device
                                )
                        )
                }
        )
)

#' @include Functions.R
NULL

Params <- R6::R6Class(
    "Params",
    list(
        w = c(0.5, 0.2, 0.2, 0.1),
        wh = 0.5,
        wd = 0.5,
        MD = 0.1,
        R = 0.5,
        fb_max = 10,
        frlow = 30,
        frhigh = 70,
        num_nodes = 100,
        num_adversaries = 0,
        num_contexts = 10,
        nodes_per_owner = 7,
        target = 8,
        observer = 1,
        target_context = 3,
        observer_context = 5,

        initialize = function() {
            self$num_adversaries <- floor(
                self$num_nodes / c(self$num_nodes + 1, 5, 2, 1.25, 1)
            )
        },

        set_frvals = function(frlow, frhigh) {
            self$frlow <- frlow
            self$frhigh <- frhigh
        }
    )
)


params <- Params$new()


Node <- R6::R6Class(
    "Node",
    list(
        id = 0,
        T = NULL,
        DT = NULL,
        Z = NULL,
        Tr = NULL,
        owner = 0,
        RT = NULL,
        friends = NULL,
        context_potential = round(runif(1, min = 1, params$num_contexts)),
        other_node_ids = NULL,
        attack_context = 0,  # Context to perform attacks on, 0 means no attacks
        CP = runif(1),
        contexts_zd = NULL,  # Contexts Z calculated on
        contexts_xd = NULL,  # Contexts transacted in

        initialize = function(owner, id) {
            self$owner <- owner
            self$id <- id
            self$other_node_ids <- setdiff(seq_len(params$num_nodes), self$id)
            self$set_vals()
        },

        set_vals = function() {
            self$T <- matrix(
                rep(rep(params$R, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$DT <- matrix(
                rep(rep(params$R, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$Z <- matrix(
                rep(rep(params$R, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$contexts_zd <- matrix(
                rep(rep(FALSE, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$contexts_xd <- matrix(
                rep(rep(FALSE, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$Tr <-  matrix(
                rep(rep(0, params$num_contexts), params$num_nodes),
                ncol = params$num_contexts,
                byrow = TRUE
            )
            self$RT <- array(  # [i, k, c]
                rep(
                    rep(
                        rep(
                            params$R, params$num_contexts
                        ),
                        params$num_nodes
                    ),
                    params$num_nodes
                ),
                dim = c(params$num_nodes, params$num_nodes, params$num_contexts)
            )
        },

        mutate_cp = function(context_potential) {
            self$context_potential <- context_potential
        },

        make_adversary = function(attack_context) {
            self$attack_context <- attack_context
        },

        make_friend = function(friend) {
            self$add_friend(friend$id)
            friend$add_friend(self$id)
        },

        add_friend = function(i) {
            self$friends <- union(self$friends, i)
        },

        calc_T = function(i, context, nodes) {
            if(context == self$attack_context) {
                self$T[i, context] <- 0
            } else {
                DT <- self$calc_DT(i, nodes)
                IndT <- self$calc_IndT(i, context, nodes)
                alpha <- `if`(
                    (DT + IndT) > 0,
                    DT / (DT + IndT),
                    0
                )
                beta <- 1 - alpha
                self$T[i, context] <- min(1, alpha * DT + beta * IndT)
            }
            return(self$T[i, context])
        },

        calc_DT = function(i, nodes) {
            return(
                `if`(
                    all(self$Tr[i, ] == 0),
                    calc_R(`if`(nodes[[i]]$owner == self$owner, 0, 1)),
                    sum(self$DT[i, ] * self$calc_WW(i))
                )
            )
        },

        calc_WW = function(i) {
            norm_Tr <- self$Tr[i, ] / sum(self$Tr[i, ])
            return(ifelse(is.nan(norm_Tr), 0, norm_Tr))
        },

        calc_IndT = function(i, context, nodes) {
            RT <- self$calc_RT(i, context, nodes)
            ids <- RT != Inf
            RT <- RT[ids]
            return(
                `if`(
                    length(RT) > 0,
                    sum(
                        RT *
                        self$calc_X(i, context, nodes)[ids]
                    ) /
                    length(RT),
                    0
                )
            )
        },

        calc_RT = function(i, context, nodes) {
            return(
                sapply(
                    self$other_node_ids,
                    function(j) {
                        `if`(
                            nodes[[j]]$contexts_xd[i, context],
                            nodes[[j]]$T[i, context],
                            `if`(
                                any(nodes[[j]]$contexts_xd[i, ]),
                                mean(nodes[[j]]$T[i, ][nodes[[j]]$contexts_xd[i, ]]),
                                Inf
                            )
                        )
                    }
                )
            )
        },

        calc_X = function(i, context, nodes) {
            Z <- self$calc_Z(context)
            Z <- Z[Z != Inf]
            return(
                params$w[[1]] * `if`(length(Z) > 0, Z, 0) +
                    params$w[[2]] * self$calc_OT(nodes) +
                    params$w[[3]] * self$calc_CP(nodes) +
                    params$w[[4]] * self$calc_R(nodes)
            )
        },

        calc_Z = function(context, nodes) {
            return(
                sapply(
                    self$other_node_ids,
                    function(i) {
                        `if`(
                            self$contexts_zd[i, context],
                            self$Z[i, context],
                            `if`(
                                any(self$contexts_zd[i, ]),
                                mean(self$Z[i, ][self$contexts_zd[i, ]]),
                                Inf
                            )
                        )
                    }
                )
            )
        },

        calc_OT = function(nodes) {
            return(
                params$wh * self$calc_H(nodes) + params$wd * self$calc_D(nodes)
            )
        },

        calc_H = function(nodes) {
            return(
                sapply(
                    self$other_node_ids,
                    function(i) {
                        calc_H(length(nodes[[i]]$friends))
                    }
                )
            )
        },

        calc_D = function(nodes) {
            af_i <- length(self$friends)
            return(
                sapply(
                    self$other_node_ids,
                    function(i) {
                        cf <- length(
                            intersect(self$friends, nodes[[i]]$friends)
                        )
                        return(cf / (af_i + length(nodes[[i]]$friends) - cf))
                    }
                )
            )
        },

        calc_CP = function(nodes) {
            return(
               sapply(
                    self$other_node_ids,
                    function(i) {
                        return(1 - nodes[[i]]$CP)
                    }
               )
            )
        },

        calc_R = function(nodes) {
            return(
                sapply(
                    self$other_node_ids,
                    function(i) {
                        return(
                            calc_R(`if`(nodes[[i]]$owner == self$owner, 0, 1))
                        )
                    }
                )
            )
        },

        transaction = function(nodes, context) {
            self$Tr[params$target, context] <- self$Tr[params$target, context] + 1
            fb <- `if`(
                nodes[[params$target]]$perform_transaction(context),
                runif(1, min = 1, max = 10),
                runif(1, min = -10, max = 0)
            )
            self$contexts_xd[params$target, context] <- TRUE
            if(self$id != params$observer) {
                self$update_DT(params$target, context, fb)
            }
            for (k in self$other_node_ids) {
                self$update_Z(params$target, k, context, fb, nodes[[k]])
            }
            invisible(self)
        },

        perform_transaction = function(context) {
            return(context <= self$context_potential)
        },

        update_DT = function(i, context, fb) {
            self$DT[i, context] <- min(
                1,
                max(
                    0,
                    self$DT[i, context] + (fb / (params$fb_max / params$MD)))
                )
            invisible(self)
        },

        update_Z = function(i, k, context, fb_s, other) {
            RT <- `if`(
                other$contexts_xd[i, context],
                other$T[i, context],
                `if`(
                    any(other$contexts_xd[i, ]),
                    mean(other$T[i, ][other$contexts_xd[i, ]]),
                    0
                )
            )
            fb <- fb_s * RT
            delta_z <- fb / (params$fb_max / params$MD)
            self$Z[k, context] <- max(0, min(1, self$Z[k, context] +
                `if`(self$RT[i, k, context] >= 0.5, delta_z, -delta_z)))
            self$contexts_zd[k, context] <- TRUE
            invisible(self)
        }
    )
)


calc_H <- function(fr) {
    `if`(
        fr > params$frhigh,
        1,
        `if`(
            fr < params$frlow,
            0,
            0.5
        )
    )
}


calc_R <- function(n) {
    1 / (2^n)
}


assign_friends <- function(nodes) {
    for (node in nodes) {
        if (node$id < params$num_nodes) {
            if (node$id == params$num_nodes - 1) {
                friend_ids <- `if`(round(runif(1)), params$num_nodes, NULL)
            } else {
                friend_ids <- sample(
                    setdiff((node$id + 1):params$num_nodes, node$id),
                    max(
                        1,
                        min(
                            params$num_nodes - node$id,
                            round(
                                rnorm(
                                    1,
                                    mean = (params$num_nodes - node$id) / 2,
                                    sd = (7 / 10) * (params$num_nodes - node$id)
                                )
                            )
                        )
                    )
                )
            }
            for (i in friend_ids) {
                node$make_friend(nodes[[i]])
            }
        }
    }
    friend_counts <- sapply(
        nodes,
        function(node) {
            return(length(node$friends))
        }
    )
    fr_sd <- sd(friend_counts)
    params$set_frvals(round(fr_sd), round(mean(friend_counts) + fr_sd))
}

perform_transactions <- function(num_trans, nodes) {
    trust_vals <- NULL
    cat_progress(
        0, num_trans, prefix = sprintf("Transaction %d/%d", 0, num_trans)
    )
    for (i in seq_len(num_trans)) {
        for (node in nodes) {
            if (node$id != params$observer) {
                context <- round(runif(1, min = 1, max = params$num_contexts))
                node$calc_T(params$target, context, nodes)
                node$transaction(
                    nodes,
                    context
                )
            }
        }
        trust_vals <- c(
            trust_vals,
            nodes[[params$observer]]$calc_T(
                params$target, params$observer_context, nodes
            )
        )
        nodes[[params$observer]]$transaction(nodes, params$observer_context)
        cat_progress(
            i, num_trans, prefix = sprintf("Transaction %d/%d", i, num_trans),
            postfix = sprintf("T = %f", tail(trust_vals, 1))
        )
    }
    return(data.frame(trusts = trust_vals, transactions = seq_len(num_trans)))
}


#' Run the simulation
#'
#' Simulation the trust model for the number of transactions
#' @keywords trust model simulation run
#' @export run_sim

run_sim <- function(num_trans) {
    nodes <- lapply(
        seq_len(params$num_nodes),
        function(i) {
            Node$new(ceiling(i / params$nodes_per_owner), i)
        }
    )
    assign_friends(nodes)
    nodes[[params$target]]$mutate_cp(10)
    trust_vals <- list()
    for (num_adv in params$num_adversaries) {
        for (i in seq_len(num_adv)) {
            nodes[[params$num_nodes + 1 - i]]$make_adversary(
                params$target_context
            )
        }
        for (node in nodes) {
            node$set_vals()
        }
        cat(
            sprintf(
                "Performing %d transactions with %d%% adversaries...\n",
                num_trans,
                num_adv / params$num_nodes * 100
            )
        )
        trust_vals[[sprintf("%d", num_adv)]] <- perform_transactions(
            num_trans,
            nodes
        )
    }
    create_plot(trust_vals)
}


create_plot <- function(trust_vals) {
    names(trust_vals) <- sprintf(
        "%d%%%s",
        params$num_adversaries / params$num_nodes * 100,
        ifelse(
            params$num_adversaries / params$num_nodes * 100 == 100,
            " Adversaries",
            ""
        )
    )
    data <- reshape2::melt(trust_vals, id.vars = "transactions")
    data$L1 <- factor(
        data$L1,
        levels = stringr::str_sort(levels(as.factor(data$L1)), numeric = TRUE)
    )
    filename <- sprintf(
        "%d-trust-eval-on-%d-in-context-%d.png",
        params$observer,
        params$target,
        params$target_context
    )
    colors <- c("blue", "red", "green", "orange", "purple")
    shapes <- c(15, 17, 18, 19, 20)
    ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = transactions,
            y = value,
            color = as.factor(L1),
            shape = as.factor(L1)
        )
    ) +
        ggplot2::geom_line() +
        # ggplot2::geom_point() +
        ggplot2::labs(
            x = "Transactions",
            y = "Trust value",
            colour = NULL,
            shape = NULL
        ) +
        ggplot2::scale_y_continuous(limits = c(-0.05, 1.05)) +
        ggplot2::scale_color_manual(values = colors) +
        # ggplot2::scale_shape_manual(values = shapes) +
        ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(
        file = filename,
        width = 7,
        height = 7,
        dpi = 320,
        type = "cairo"
    )
    cat(sprintf("Saved plot of trust to %s\n", filename))
}

# Compute the probability of a occuring among a, b, and c
compute_probability <- function(a, b, c) {
        return(sum(a, 1) / sum(a, b, c, 3))
}


# Compute the entropy from the given probabilities
compute_entropy <- function(probabilities) {
        return(-sum(ent_unit(probabilities)))
}


# Calculate the small unit of entropy
ent_unit <- function(probability) {
        return(probability * log(probability, base = 3))
}


# Do the same as ent_unit but divide the contexts of the log by divider
ent_unit_div <- function(probability, divider) {
        return(probability * log(probability / divider, base = 3))
}


# Find the trust of a device given amounts of trusted, distrusted, and unknown
# services they have provided
compute_trust <- function(trust, distrust, unknown) {
        prob_trust <- compute_probability(trust, distrust, unknown)
        prob_distrust <- compute_probability(distrust, trust, unknown)
        prob_unknown <- compute_probability(unknown, trust, distrust)

        if ((prob_trust > prob_unknown) && (prob_unknown >= prob_distrust)) {
                return(1 - compute_entropy(c(prob_trust, prob_distrust, prob_unknown)))
        } else if ((prob_trust > prob_distrust) && (prob_distrust > prob_unknown)) {
                return(1 - abs(ent_unit(prob_trust) +
                        ent_unit_div(prob_unknown + prob_distrust, 2)))
        } else if (prob_unknown >= max(prob_trust, prob_distrust)) {
                return(abs(ent_unit(prob_trust) +
                        ent_unit_div(prob_unknown + prob_distrust, 2)) - 1)
        } else if ((prob_distrust >= prob_unknown) &&
                (prob_unknown >= prob_trust)) {
                return(compute_entropy(c(prob_trust, prob_distrust, prob_unknown)) - 1)
        } else {
                return(abs(ent_unit(prob_distrust) +
                        ent_unit_div(prob_unknown + prob_trust, 2)) - 1)
        }
}


# Find the weighted average of the context values (given a vector of one of the 4)
weighted_avg_context <- function(contexts) {
        context_latest <- tail(contexts, 1)
        factor_forget <- params$theta_i**abs(context_latest - head(contexts, -1))
        return((context_latest + sum(factor_forget * head(contexts, -1))) /
                (1 + sum(factor_forget)))
}


# Take a vector of the contexts and return the weighted average
find_weighted_context <- function(contexts) {
        return(
                apply(
                        matrix(
                                contexts,
                                nrow = length(params$context_weights)
                        ),
                        1,
                        weighted_avg_context
                )
        )
}


# Find the distance between the target context, and the weighted context
context_distance <- function(context_target, context_weighted) {
        return(
                sqrt(
                        sum(
                                params$context_weights * (context_target - context_weighted)**2
                        )
                )
        )
}


# Estimate how trusted a node will be for the target context
estimate_trust <- function(context_target, context_weighted, trust_current) {
        # NOTE: The conditions for the prods have been flipped from the paper
        if (trust_current <= 0) {
                return(
                        max(
                                -1,
                                trust_current *
                                        prod(
                                                2 - params$eta[[4]]**
                                                        delta(
                                                                context_target,
                                                                context_weighted,
                                                                context_target < context_weighted
                                                        )
                                        ) *
                                        prod(
                                                params$eta[[5]]**
                                                        delta(
                                                                context_target,
                                                                context_weighted,
                                                                context_target > context_weighted
                                                        )
                                        )
                        )
                )
        }
        return(
                min(
                        1,
                        trust_current *
                                prod(
                                        params$eta[[2]]**
                                                delta(
                                                        context_target,
                                                        context_weighted,
                                                        context_target < context_weighted
                                                )
                                ) *
                                prod(
                                        2 - params$eta[[3]]**
                                                delta(
                                                        context_target,
                                                        context_weighted,
                                                        context_target > context_weighted
                                                )
                                )
                )
        )
}


# A function used within the trust estimation
delta <- function(context_target, context_weighted, cond) {
        return(
                (params$context_weights[cond] *
                        abs(context_target[cond] - context_weighted[cond])) /
                        params$impact_factor
        )
}


# Calculate a weighted trust
weighted_trust <- function(trust_estimate, trust, distrust, unknown) {
        prob_trust <- compute_probability(trust, distrust, unknown)
        prob_distrust <- compute_probability(distrust, trust, unknown)
        prob_unknown <- compute_probability(unknown, trust, distrust)

        return(
                `if`(
                        prob_trust > max(prob_unknown, prob_distrust),
                        params$alpha,
                        `if`(
                                prob_unknown >= max(prob_trust, prob_distrust) &&
                                        prob_unknown != prob_distrust,
                                params$beta,
                                params$gamma
                        )
                ) * trust_estimate
        )
}


# Calculate the direct trust
direct_trust <- function(trusts, context_target, context_weighted) {
        return(sum(omega(context_weighted, context_target) * trusts))
}


# Calculate the indirect trust
indirect_trust <- function(trusts, reputations, contexts,
                           context_weighted, context_cached) {
        omega_weighted <- omega(context_weighted, contexts)
        omega_cached <- omega(context_cached, contexts)
        print(sprintf("context weighted: %f", context_weighted))
        print(sprintf("context cached: %f", context_cached))
        print()
        return(
                sum(omega_weighted * omega_cached * reputations * trusts) /
                        sum(omega_weighted)
        )
}


# A function used within the indirect and direct trust calculations
omega <- function(context_weighted, context_target) {
        return(
                params$eta[[1]]**(
                        apply(
                                matrix(
                                        context_target,
                                        ncol = length(context_weighted),
                                        byrow = T
                                ),
                                1,
                                function(c) {
                                        return(context_distance(context_weighted, c))
                                }
                        ) / params$delta
                )
        )
}


# Calculate the expected value of change in the trust
trend_of_trust <- function(trust0, trust1, context0, context1) {
        return(
                `if`(
                        trust0 == 0,
                        trust1,
                        trust1 -
                                params$eta[[1]]**(context_distance(context1, context0) /
                                        params$delta)
                                * trust0
                )
        )
}


# Calculate a new reputation value for a service provider
reputation_combination <- function(context_old, context_target, context_new,
                                   reputation_old, reputation) {
        omega_new_old <- omega(context_new, context_old)
        omega_new_target <- omega(context_new, context_target)
        return(
                omega_new_old * reputation_old + omega_new_target * params$rho**
                        `if`(
                                reputation_old * reputation > 0,
                                omega_new_old * abs(reputation_old),
                                1 - omega_new_old * abs(reputation_old)
                        ) * reputation
        )
}


acceptable_rec <- function(c_cached, c_recced, t_recced) {
        return(
                abs(
                        t_recced *
                                params$eta[[1]]**(
                                        context_distance(c_cached, c_recced) /
                                                params$delta
                                )
                ) <
                        params$trust_rep_threshold + params$trust_rep_adj_range
        )
}


get_context_index <- function(i) {
        con_len <- length(params$context_weights)
        return((con_len * (i - 1) + 1):(con_len * i))
}


minimax <- function(x, minima, maxima) {
        return(max(minima, min(maxima, x)))
}

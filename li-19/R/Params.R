Params <- R6::R6Class(
    "Params",
    list(
        number_nodes = 21,
        number_good_nodes = 0,
        number_service_providers = 1,
        signal_radius = 100,
        dev_signal_radius = 14,
        max_number_contacts = 100,
        init_reputation = 0.01,
        rep_self = 1,
        trust_new_contact = 0,
        trust_rep_threshold = 0,
        trend_threshold = 0.01,
        trust_rep_adj_range = 0.001,
        sp_ground_trust = 1,
        max_capability = 100,
        map_width = 100,
        map_height = 100,
        max_velocity = 10,
        time_now = 1,
        context_weights = c(0.3, 0.2, 0.4, 0.1),
        eta = c(0.95, 0.7, 0.5, 0.5, 0.7),
        alpha = 0.3,
        beta = 0.3,
        gamma = 0.8,
        rho = 0.1,
        delta = 0.8,
        delta_a = -0.001,
        p_r = 1,
        theta_i = 0.8,
        impact_factor = 1,
        eta_i = 1,
        gap_factor = 2**-1,
        min_trans = 1,
        max_trans = 1,
        img_width = NULL,
        img_height = NULL,
        compression_factor = Inf,
        number_adversaries = 0,
        adversary_type = BadMouther,
        contacts_per_node = 10,
        rand_context = F,
        number_observer_contacts = 10,
        tdu_increment = 1,
        observer_targeted = F,
        target_location = c(1, 1),
        target_capability = 50,
        target_velocity = 5,

        initialize = function() {
            self$number_good_nodes <- self$number_nodes - (self$number_adversaries + 1)
            self$img_width <- ceiling(5**(1 - self$map_width / 1000)) * self$map_width
            self$img_height <- ceiling(5**(1 - self$map_height / 1000)) * self$map_height
            invisible(self)
        },

        increment_time = function() {
            self$time_now <- self$time_now + 1
            invisible(self)
        },

        configure = function(data) {
            self$number_nodes <- data$number_nodes
            self$number_service_providers <- data$number_service_providers
            self$signal_radius <- data$signal_radius
            self$dev_signal_radius <- data$dev_signal_radius
            self$max_number_contacts <- data$max_number_contacts
            self$init_reputation <- data$init_reputation
            self$rep_self <- data$rep_self
            self$trust_new_contact <- data$trust_new_contact
            self$trust_rep_threshold <- data$trust_rep_threshold
            self$trend_threshold <- data$trend_threshold
            self$trust_rep_adj_range <- data$trust_rep_adj_range
            self$sp_ground_trust <- data$sp_ground_trust
            self$max_capability <- data$max_capability
            self$map_width <- data$map_width
            self$map_height <- data$map_height
            self$max_velocity <- data$max_velocity
            self$time_now <- data$time_now
            self$context_weights <- data$context_weights
            self$eta <- data$eta
            self$alpha <- data$alpha
            self$beta <- data$beta
            self$gamma <- data$gamma
            self$rho <- data$rho
            self$delta <- data$delta
            self$delta_a <- data$delta_a
            self$p_r <- data$p_r
            self$theta_i <- data$theta_i
            self$impact_factor <- data$impact_factor
            self$eta_i <- data$eta_i
            self$gap_factor <- data$gap_factor
            self$min_trans <- data$min_trans
            self$tdu_increment <- data$tdu_increment
            self$max_trans <- data$max_trans
            self$target_location <- data$target_location
            self$target_capability <- data$target_capability
            self$target_velocity <- data$target_velocity

            self$compression_factor <- `if`(
                data$compression_factor <= 0,
                Inf,
                data$compression_factor
            )
            self$number_adversaries <- data$number_adversaries
            self$adversary_type <- get_adversary_type(data$adversary_type)
            self$contacts_per_node <- data$contacts_per_node
            self$rand_context <- data$rand_context
            self$observer_targeted <- data$observer_targeted
            self$number_good_nodes <- self$number_nodes - (self$number_adversaries + 1)
            self$img_width <- ceiling(5**(1 - self$map_width / 1000)) * self$map_width
            self$img_height <- ceiling(5**(1 - self$map_height / 1000)) * self$map_height
            self$number_observer_contacts <- data$number_observer_contacts
            invisible(self)
        }
    )
)


get_adversary_type <- function(ad_type) {
    if (grepl("BadMouther", ad_type)) {
        return(BadMouther)
    } else if (grepl("GoodMouther", ad_type)) {
        return(GoodMouther)
    } else {
        return(ContextSetter)
    }
}

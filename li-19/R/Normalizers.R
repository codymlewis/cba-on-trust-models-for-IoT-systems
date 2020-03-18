normalize <- function(context) {
    normalizers <- c(
        normalize_time,
        normalize_capability,
        normalize_location,
        normalize_velocity
    )

    return(
        sapply(
            seq_len(length(context)),
            function(i) {
                normalizers[[i]](context[[i]])
            }
        )
    )
}


normalize_time <- function(time) {
    return(time)
}


normalize_capability <- function(capability) {
    return(1 - (capability / params$max_capability))
}


normalize_location <- function(distance) {
    return(1 - (distance / sqrt(params$map_width**2 + params$map_height**2)))
}


normalize_velocity <- function(velocity) {
    return(1 - (velocity / params$max_velocity))
}

#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-02-13
# Description:
# Show the relationship between capability, QR, and Reputation

# The max values of service and capability respectively
S_MAX = 101
C_MAX = 101

# Get a scalar distance between 2 values
find_dist <- function(target, current) {
    return(abs(target - current))
}

# Find the distance between a report's context and a target context
report_dist <- function(c_j, s_j, c_target, s_target, eta, note,
			dS_max_sq, dC_max_sq, S_max, C_max) {
    shared_term = sqrt(
        (dS_max_sq + dC_max_sq) *
        (
         (find_dist(s_target, s_j)**2 /
          dS_max_sq) +
             (find_dist(c_target, c_j)**2 /
              dC_max_sq)
        )
    )
    unique_term = `if`(
        note >= 0,
        sqrt(
            (dS_max_sq + dC_max_sq) *
            (
             ((S_max - s_j) /
              (S_max - (s_target - eta)))**2 +
             (c_j /
              (c_target + eta))**2
            )
        ),
        sqrt(
            (dS_max_sq + dC_max_sq) *
            (
             ((C_max - c_j) /
              (C_max - (c_target - eta)))**2 +
             (s_j / (s_target + eta))**2
            )
        )
    )
    return(min(shared_term, unique_term))
}

find_s <- function(note_j) {
    return((1 / 2) * (note_j**2 - note_j))
}

# Find the weight of a report
find_weight <- function(lambda, theta, note, report_time, distance, time) {
    theta_exp = ((find_s(note) + 1) * (time - report_time))
    return((lambda ** distance) * (theta ** theta_exp))
}

# Give a value stating the significance of older occurances
find_c_i <- function(theta, t_1, t_i) {
    return(theta ** (t_1 - t_i))
}

# Find a quality of reccomendation for a given report weight
find_qr <- function(weight, client_note, theta, time, node_note,
                    client_qr, node_qrs, node_qr_times) {
    QR = 1
    C_F = weight * client_qr
    QRXF = C_F * (-abs(node_note - client_note))
    numerator=denominator=0
    numerator = sum(sapply(1:length(node_qrs),
        function(i) {
            c_i = find_c_i(theta, node_qr_times[[1]],
                           node_qr_times[[i]])
            c_i * node_qrs[[i]] + QRXF
        }
    ))
    denominator = sum(sapply(1:length(node_qrs),
        function(i) {
            c_i = find_c_i(theta, node_qr_times[[1]],
                           node_qr_times[[i]])
            c_i + abs(C_F)
        }
    ))
    QR = `if`(
        denominator == 0,
        0,
        numerator / denominator
    )
    if(QR < -1) {
       QR = -1
    } else if(QR > 1) {
        QR = 1
    }
    return(QR)
}

# Calculate the reputation of a server
calculate_reputation <- function(theta, client_notes, client_qrs,
                                 transactions, time, node_qr_times) {
    sum = 0
    for(i in 1:transactions) {
        c_i = find_c_i(theta, time, node_qr_times[[i]])
        sum = sum + c_i * client_notes[[i]] * client_qrs[[i]]
    }
    return(sum)
}

# Give a vector of notes produce during operation of the system
take_notes <- function(c, c_target, s, s_target, client_qrs) {
    correct_notes = rep(
        `if`(
            c < c_target,
            `if`(s < s_target, -1, 0),
            `if`(s < s_target, 0, 1),
        ),
        each=length(client_qrs)
    )
    ifelse(
        runif(length(client_qrs)) < client_qrs,
        correct_notes,
        setdiff(c(-1, 0, 1), correct_notes)[[floor(runif(1, min=1, max=3))]]
    )
}

plot_reputation <- function(reputations) {
    plot(
        reputations,
        xlab = "Number of Transactions",
        ylab = "Reputation Value",
        main = "Reputation of the Node in a Trust Model Network",
        type = "l",
        col = "blue"
    )
}

plot_cap_qr <- function(final_qrs) {
    png("graphs/cap_qr.png")
    plot(
        x = 1:100,
        y = final_qrs,
        xlab = "Capability Values",
        ylab = "Final QRs",
        main = "Capability vs. QR when Good Mouthing a Good Service",
        col = "blue"
    )
    dev.off()
}

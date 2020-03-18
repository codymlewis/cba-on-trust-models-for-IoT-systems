#!/usr/bin/env Rscript
# Author: Cody Lewis
# Description:
# Console interface showing how the reputation equation works

library(optigrab)

source("ReputationQR.r")

# The main interface
main <- function() {
    cat("Simulating the effects of varying capabilities on the system...\n")
    s_target = as.numeric(opt_get(c("s-target", "st"), default=50))
    c_target = as.numeric(opt_get(c("c-target", "ct"), default=50))
    s_j = as.numeric(opt_get(c("s-j", "sj"), default=50))
    eta = as.numeric(opt_get(c("eta", "e"), default=50))
    node_note = as.numeric(opt_get(c("node-note", "n"), default=1))
    lambda = as.numeric(opt_get(c("lambda", "l"), default=0.7))
    theta = as.numeric(opt_get(c("theta", "t"), default=0.7))
    number_of_transactions = as.numeric(opt_get(c("transactions", "tr"), default=300))
    final_qrs = rep(1, each=100)
    reputations = matrix(
        rep(1, each=((number_of_transactions + 1) * 100)), nrow=100
    )
    weights_text = rep("", each=100)
    for(c_j in 1:100) {
        node_qrs = c(1)
        node_qr_times = c(1)
        time = 1
        client_qrs = runif(number_of_transactions)
        client_notes = take_notes(c_j, c_target, s_j, s_target, client_qrs)
        for(transactions in 1:number_of_transactions) {
            if((transactions %% 30) == 0) {
                time = time + 1
            }
            dS_max_sq = find_dist(S_MAX, s_target)**2
            dC_max_sq = find_dist(C_MAX, c_target)**2
            d = report_dist(c_j, s_j, c_target, s_target, eta, node_note,
                            dS_max_sq, dC_max_sq, S_MAX, C_MAX)
            w = find_weight(lambda, theta, node_note, time, d, time)
            qr = find_qr(w, client_notes[[transactions]], theta, time,
                         node_note, client_qrs[[transactions]], node_qrs,
                         node_qr_times)
            if(d >= sqrt(dS_max_sq + dC_max_sq)) {
                weights_text[[c_j]] = "Reports produced in the transaction phase of this attack will have no impact on the client's QR as the distance of the report is out of bounds"
            } else {
                weights_text[[c_j]] = sprintf("Reports produced in the transaction phase of this attack will have a weight of %f", w)
            }
            node_qrs = c(qr, node_qrs)
            node_qr_times = c(time, node_qr_times)
            reputations[c_j, transactions + 1] =
                calculate_reputation(
                    theta, client_notes, client_qrs,
                    transactions, time, node_qr_times
                )
        }
        final_qrs[[c_j]] = node_qrs[[1]]
        cat(
            sprintf(
                "Completed %d transactions for c_j: %d\n",
                number_of_transactions, c_j
            )
        )
    }
    dir.create("./graphs", showWarnings=FALSE)
    for(c_j in 1:100) {
        png(sprintf("graphs/%d_reputation_evolution.png", c_j))
        plot_reputation(reputations[c_j, ])
        dev.off()
        cat(sprintf("%d\t%s\n", c_j, weights_text[[c_j]]))
    }
    plot_cap_qr(final_qrs)
    cat("Done. Created a few plots in the ./graphs directory\n")
}

main()

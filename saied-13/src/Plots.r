# A plotting library for the the trust model
#
# Author: Cody Lewis
# Date: 2019-05-03


library(ggplot2)

# Plot the QRs of nodes over time
plot.nodes <- function(nodes) {
    ids = c()
    recommendations.number = c()
    QRs = c()
    malicious.state = c()

    for(node in nodes) {
        node.recommendations = length(node$QR)
        ids = c(ids, rep(node$id, node.recommendations))
        recommendations.number = c(
            recommendations.number, 1:node.recommendations
        )
        QRs = c(QRs, rev(node$QR))
        malicious.state = c(
            malicious.state, rep(
                `if`(
                    is(node, "Node.BadMouther"),
                    "Malicious Reporter",
                    "Non-Malicious Reporter"
                ),
                node.recommendations
            )
        )
    }
    data <- data.frame(
        ids=ids,
        recommendations=recommendations.number,
        QRs=QRs,
        malicious.state=malicious.state
    )

    ggplot(data=data, aes(x=recommendations, y=QRs, group=ids)) +
        geom_line(aes(colour=malicious.state)) +
        labs(
            title="Quality of Recommendations of Nodes",
            x="Number of Recommendations",
            y="Quality of Recommendations",
            colour=NULL
        ) +
        malicious_indicator() +
        y_limit() +
        theme(legend.position = "bottom")
}

# Plot all of the trust values of all nodes over time
plot.trust <- function(nodes) {
    ids = c()
    transactions = c()
    trust = c()
    malicious.state = c()

    for(node in nodes) {
        number.transactions = length(node$trust)
        ids = c(ids, rep(node$id, number.transactions))
        transactions = c(transactions, 1:number.transactions)
        trust = c(trust, node$trust)
        malicious.state = c(
            malicious.state, rep(
                `if`(
                    is(node, "Node.BadMouther"),
                    "Malicious Reporter",
                    "Non-Malicious Reporter"
                ),
                number.transactions
            )
        )
    }
    data = data.frame(
        ids=ids,
        transactions=transactions,
        trust=trust,
        malicious.state=malicious.state
    )

    ggplot(data=data, aes(x=transactions, y=trust, group=ids)) +
        geom_point(aes(colour=malicious.state)) +
        malicious_indicator() +
        labs(
            title="Trust Values of the Node Services",
            x="Number of Transactions",
            y="Trust Value",
            colour = NULL
        ) +
        y_limit() +
        theme(legend.position = "bottom")
}

# plot the trust of a single node
plot.node.trust <- function(node, total.nodes=200) {
    if(length(node$trust) > 0) {
        data = data.frame(transactions=1:length(node$trust), trust=node$trust)
        is.targeted = node$id <= floor(total.nodes / 6.6)

        return(
            ggplot(data=data, aes(x=transactions, y=trust)) +
                geom_line(colour=`if`(is.targeted, "purple", "green")) +
                labs(
                    title=sprintf(
                        "Trust of a %s Group Node Over Time",
                        `if`(is.targeted, "Target", "Normal")
                    ),
                    x="Number of Transactions",
                    y="Trust Value",
                    colour = NULL
                ) +
                y_limit() +
                theme(legend.position = "bottom")
        )
    } else {
        return(NA)
    }
}

# Plot the final Quality of recommendations
plot.QRs.final <- function(nodes) {
    ids = c()
    QRs.final = c()
    malicious.state = c()

    for(node in nodes) {
        ids = c(ids, node$id)
        QRs.final = c(QRs.final, head(node$QR, 1))
        malicious.state = c(
            malicious.state,
            `if`(
                is(node, "Node.BadMouther"),
                "Malicious Reporter",
                "Non-Malicious Reporter"
            )
        )
    }
    data = data.frame(
        ids=ids,
        QRs.final=QRs.final,
        malicious.state=malicious.state
    )

    ggplot(data=data, aes(x=ids, y=QRs.final)) +
        geom_point(aes(colour=malicious.state)) +
        malicious_indicator() +
        labs(
            title="Final Quality of Recommendations of the Nodes",
            x="Node ID",
            y="Final Quality of Recommendation",
            colour=NULL
        ) +
        y_limit() +
        theme(legend.position = "bottom")
}

# Plot the average trust over at each point of time of a target group and a
# normal group
plot.trust.targeted <- function(nodes, number.transactions) {
    number.targeted = floor(length(nodes) / 6.6)
    target.group = 1:number.targeted
    normal.group = number.targeted + 1:2 * number.targeted
    target.group.trusts = rep(0, number.transactions)
    normal.group.trusts = rep(0, number.transactions)

    for(i in 1:number.transactions) {
        sum.trust = 0
        for(target.node in nodes[target.group]) {
            sum.trust = sum.trust + target.node$trust[[i]]
        }
        target.group.trusts[[i]] = sum.trust / length(target.group)
        sum.trust = 0
        for(normal.node in nodes[normal.group]) {
            sum.trust = sum.trust + normal.node$trust[[i]]
        }
        normal.group.trusts[[i]] = sum.trust / length(normal.group)
    }
    trusts = c(target.group.trusts, normal.group.trusts)
    is_target_group = c(
        rep("Target Group", each=number.transactions),
        rep("Normal Group", each=number.transactions)
    )
    data = data.frame(
        trusts=trusts,
        transactions = c(1:number.transactions, 1:number.transactions),
        is_target_group = is_target_group
    )

    ggplot(data=data, aes(x=transactions, y=trusts, group=is_target_group)) +
        geom_line(aes(color=is_target_group)) +
        labs(
            title="Average Trust of the Nodes Over Time",
            x="Number of Transactions",
            y="Average Trust Value",
            colour = NULL
        ) +
        target_indicator() +
        y_limit() +
        theme(legend.position = "bottom")
}

# Plot the mean trust of the node within the network without accounting for
# targeting
plot.trust.mean <- function(nodes, number.transactions) {
    trusts = rep(0, number.transactions)

    for(i in 1:number.transactions) {
        sum.trust = 0
        for(node in nodes) {
            sum.trust = sum.trust + node$trust[[i]]
        }
        trusts[[i]] = sum.trust / length(nodes)
    }

    data = data.frame(trusts=trusts, transactions=1:number.transactions)
    ggplot(data=data, aes(x=transactions, y=trusts)) +
        geom_line(color="green") +
        labs(
            title="Average Trust of the Nodes Over Time",
            x="Number of Transactions",
            y="Average Trust Value",
            colour = NULL
        ) +
        y_limit() +
        theme(legend.position = "bottom")
}

# Colourise Malicious and non Malicious nodes for ggplot
malicious_indicator <- function() {
    return(
        scale_color_manual(
            breaks=c(
                "Malicious Reporter",
                "Non-Malicious Reporter"
            ),
            values=c("red", "blue")
        )
    )
}

# Provide a limit on the y-axis for ggplot
y_limit <- function() {
    return(ylim(c(-1.1, 1.1)))
}

# Indicate whether the plot is the target group or not
target_indicator <- function() {
    return(
        scale_color_manual(
            breaks=c("Target Group", "Normal Group"),
            values=c("green", "purple")
        )
    )
}

# Save a graph
graph.save <- function(filename) {
    ggsave(file=sprintf("./graphs/%s", filename), dpi=320)
}

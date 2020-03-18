# Definition of the Trust Model
#
# Author: Cody Lewis
# Date: 2019-05-02

source("Report.r")
source("Node.r")
source("Plots.r")

SPLITTING <- 4

# The Trust Manager class
TrustManager <- setRefClass(
    "TrustManager",

    fields=list(
        nodes="list",
        nodes.all="list",
        eta="numeric",
        theta="numeric",
        lambda="numeric",
        service.max="numeric",
        capability.max="numeric",
        reputation.threshold="numeric",
        QR.initial="numeric",
        services="numeric",
        id.nodemon.normal="numeric",
        id.nodemon.malicious="numeric",
        type.calc="list",
        threshold.directs="numeric",
        threshold.indirects="numeric",
        altering.notes="logical",
        disregard.qr="logical",
        disregard.multiplier="numeric"
    ),

    methods=list(
        init = function(number.nodes, percent.constrained, percent.poorwitness,
                        percent.malicious, percent.malicious.reporter,
                        type.malicious.reporter, targeted, type.calc,
                        disregard.multiplier) {
            "Initialize the network to the specifications of the arguments"
            services <<- c(1, 16, 33, 50, 66, 82, 100)
            ids <- seq(1, number.nodes)
            service.and.capability = assign.contexts(
                number.nodes, ids, percent.constrained, service.max,
                capability.max, targeted
            )
            # Assign note taking accuracy
            ids.poorwitness = sample(ids, percent.poorwitness * number.nodes)
            noteacc = rep(1.0, number.nodes)
            noteacc[ids.poorwitness] = runif(length(ids.poorwitness))
            # Assign the malicious node's ids
            ids.malicious = sample(ids, percent.malicious * number.nodes)
            ids.malicious.reporter = sample(
                ids, percent.malicious.reporter * number.nodes
            )
            id.nodemon.malicious <<- sample(ids.malicious, 1)
            id.nodemon.normal <<- sample(ids[!ids %in% ids.malicious], 1)
            assign.nodes(
                ids, ids.malicious, ids.malicious.reporter,
                type.malicious.reporter, service.and.capability[[1]],
                service.and.capability[[2]], noteacc, number.nodes, type.calc
            )
            type.calc <<- type.calc
            if(type.calc[[SPLITTING]]) {
                threshold.directs <<- 10
                threshold.indirects <<- -0.5
            }
            altering.notes <<- type.calc[[3]]
            disregard.qr <<- type.calc[[5]]
            disregard.multiplier <<- disregard.multiplier
        },

        assign.nodes = function(ids, ids.malicious, ids.malicious.reporter,
                                type.malicious, service, capability, noteacc,
                                number.nodes, type.calc) {
            "Create and assign values to the nodes"
            for(id in ids) {
                is.malicious = id %in% ids.malicious
                node.type = Node
                if(id %in% ids.malicious.reporter) {
                    if(type.malicious == "bm") {
                        node.type = Node.BadMouther
                    } else if(type.malicious == "bmss") {
                        node.type = Node.BadMouther.ServiceSetter
                    } else if(type.malicious == "bmcs") {
                        node.type = Node.BadMouther.CapabilitySetter
                    } else if(type.malicious == "bmtd") {
                        node.type = Node.BadMouther.TimeDecayer
                    } else if(type.malicious == "bmcstd") {
                        node.type = Node.BadMouther.CapabilitySetter.TimeDecayer
                    } else if(type.malicious == "bmsstd") {
                        node.type = Node.BadMouther.ServiceSetter.TimeDecayer
                    } else if(type.malicious == "bmcsss") {
                        node.type = Node.BadMouther.CapabilitySetter.ServiceSetter
                    } else if(type.malicious == "bmcssstd") {
                        node.type = Node.BadMouther.CapabilitySetter.ServiceSetter.TimeDecayer
                    }
                }
                nodes[[id]] <<- node.type(
                    id=id, service=service[id], capability=capability[id],
                    noteacc=noteacc[id], QR=QR.initial, is.malicious,
                    number.nodes, type.calc
                )
                nodes.all[[id]] <<- nodes[[id]]
            }
        },

        info.gather = function(epochs, time.current) {
            "Induce random artificial interactions between the nodes"
            for(epoch in 1:epochs) {
                client = nodes[[round(runif(1, min=1, max=length(nodes)))]]
                server = nodes[[round(runif(1, min=1, max=length(nodes)))]]
                service = services[round(runif(1, min=1, max=length(services)))]
                capability = round(runif(1, min=1, max=capability.max))
                client$make.report(server, service, capability, time.current)
            }
        },

        select.entity = function(id.client, target.service, target.capability,
                                 time.current) {
            "Perform the entity selection operations, and return the trusted list"
            trust = rep(0, length(nodes))
            t = find.t(target.service, target.capability, service.max,
                       capability.max)

            for(node in nodes) {
                if(type.calc[[SPLITTING]]) {
                    trust[[node$id]] = calc.trust.alt(
                        id.client,
                        target.service,
                        target.capability,
                        time.current,
                        t,
                        node
                    )
                    node$trust[[length(node$trust) + 1]] <- trust[[node$id]]
                } else {
                    trust[[node$id]] = calc.trust(id.client, target.service,
                                                  target.capability,
                                                  time.current, t, node)
                    node$trust[[length(node$trust) + 1]] <- trust[[node$id]]
                }
            }
            data.trust = data.frame(id=1:length(nodes), trust=trust)
            ids.trusted = data.trust[order(-data.trust$trust),]$id

            return(ids.trusted)
        },

        calc.trust = function(id.client, target.service, target.capability,
                              time.current, t, node) {
            "Calculate the trust of a particular node"
            numerator = 0
            denominator = 0
            for(report in node$reports) {
                if(report$disregard) {
                    if(altering.notes) {
                        note = -1
                    } else {
                        next
                    }
                }

                dist = report.distance(report, target.service,
                                       target.capability, service.max,
                                       capability.max, eta)
                if(dist < t) {
                    weight = report.weigh(
                        report, dist, lambda, theta, time.current
                    )
                    numerator = numerator + weight *
                        nodes[[report$issuer]]$QR[[1]] * report$note
                    denominator = denominator + weight
                }
            }
            return(`if`(denominator == 0, 0, numerator / denominator))
        },

        calc.trust.alt = function(id.client, target.service, target.capability,
                                  time.current, t, node) {
            "Perform an alternate form of the trust calculation"
            direct.numerator = 0
            direct.denominator = 0
            indirect.numerator = 0
            indirect.denominator = 0
            count.direct.recs = 0
            for(report in node$reports) {
                note = report$note
                if(report$disregard) {
                    if(altering.notes) {
                        note = -1
                    } else {
                        next
                    }
                }
                dist = report.distance(
                        report, target.service, target.capability,
                        service.max, capability.max, eta
                )
                if(dist < t) {
                    weight = report.weigh(report, dist, lambda, theta,
                                          time.current)
                    if(report$issuer == id.client) {
                        direct.numerator = direct.numerator + weight *
                            nodes[[report$issuer]]$QR[[1]] * note
                        direct.denominator = direct.denominator + weight
                        count.direct.recs = count.direct.recs + 1
                    } else {
                        indirect.numerator = indirect.numerator + weight *
                            nodes[[report$issuer]]$QR[[1]] * note
                        indirect.denominator = indirect.denominator + weight
                    }
                }
            }
            indirect.trust = `if`(
                indirect.denominator == 0,
                0,
                indirect.numerator / indirect.denominator
            )
            if(count.direct.recs < threshold.directs ||
               indirect.trust < threshold.indirects) {
                return(indirect.trust)
            }
            return(
                `if`(
                    direct.denominator == 0,
                    0,
                    direct.numerator / direct.denominator
                )
            )
        },

        transaction = function(id.client, id.server, target.service,
                               target.capability, time.current) {
            "Perform a transaction"
            server = nodes[[id.server]]
            nodes[[id.client]]$make.report(
                server, target.service, target.capability, time.current
            )
            if(length(server$reports) > 5 * length(nodes)) {
                server$reports <- tail(server$reports, 5 * length(nodes))
            }
            report = server$reports[[length(server$reports)]]
            report$server <- TRUE
            client.note = report$note

            return(client.note)
        },

        update.QRs = function(id.client, id.server, client.note,
                              target.service, target.capability,
                              time.current) {
            "Update the QRs of the witness nodes"
            t = find.t(
                target.service, target.capability, service.max, capability.max
            )

            for(report in nodes[[id.server]]$reports) {
                dist = report.distance(
                    report, target.service, target.capability, service.max,
                    capability.max, eta
                )
                if(dist < t) {
                    C.client = report.weigh(
                            report, dist, lambda, theta,time.current
                        ) * nodes[[id.client]]$QR[[1]]
                    if(disregard.qr && report$disregard && C.client > 0) {
                        C.client = -C.client * disregard.multiplier
                    }
                    r = -abs(report$note - client.note) + 1
                    QR.client.witness = C.client * r
                    node.witness = nodes[[report$issuer]]
                    numerator = 0
                    denominator = 0
                    for(index.QR in 1:length(node.witness$QR)) {
                        c.i = find.c.i(
                            theta,
                            node.witness$time.QR[[1]],
                            node.witness$time.QR[[index.QR]]
                        )
                        numerator = numerator + c.i *
                            node.witness$QR[[index.QR]] + QR.client.witness
                        denominator = denominator + c.i + abs(C.client)
                    }
                    node.witness$QR <- c(
                        `if`(denominator == 0, 0, numerator / denominator),
                        node.witness$QR
                    )
                    node.witness$time.QR <- c(
                        time.current, node.witness$time.QR
                    )
                }
            }
        },

        update.reputation = function(id.server) {
            "Update the reputation of the server"
            node.server = nodes[[id.server]]
            reputation = 0

            for(report in node.server$reports) {
                # if(disregard.qr && report$disregard) {
                #     next
                # }
                if(report$server) {
                    c.i = find.c.i(
                        theta,
                        nodes[[report$issuer]]$time.QR[[1]],
                        report$issuer.time.QR
                    )
                    reputation = reputation + c.i * report$note *
                        report$issuer.QR
                }
            }
            node.server$reputation <- reputation
            if(reputation < reputation.threshold) {
                nodes <<- nodes[!nodes %in% id.server]
            }
        },

        phase = function(epochs.bootstrap, time.current) {
            "Perform a single set of phases"
            info.gather(epochs.bootstrap, time.current)
            id.client = round(runif(1, min=1, max=length(nodes)))
            target.service = services[
                round(runif(1, min=1, max=length(services)))
            ]
            target.capability = round(
                runif(1, min=1, max=capability.max)
            )
            id.server = select.entity(
                id.client, target.service, target.capability, time.current
            )[[1]]
            client.note = transaction(
                id.client,
                id.server,
                target.service,
                target.capability,
                time.current
            )
            update.QRs(
                id.client,
                id.server,
                client.note,
                target.service,
                nodes[[id.server]]$capability,
                time.current
            )
            update.reputation(id.server)
        }
    )
)

# Calculate c_i, a time decay value for Quality of Recommendation
find.c.i = function(theta, time.latest, time.QR) {
    return(theta**(time.latest - time.QR))
}

# Assign the service and capability values for each node
assign.contexts <- function(number.nodes, ids, percent.constrained,
                            service.max, capability.max, targeted) {
    if(targeted) {
        target.group = 1:floor(number.nodes / 6.6)
        ids.constrained <- sample(ids, percent.constrained * number.nodes)
        service <- rep(service.max, number.nodes)
        services.nontarget = c(1:44, 56:service.max)
        service[ids.constrained] <- sample(
            services.nontarget, length(ids.constrained), replace=TRUE
        )
        service[target.group] <- round(
            runif(length(target.group), min=45, max=55)
        )
        capability <- rep(capability.max, number.nodes)
        capabilities.nontarget = c(1:44, 56:capability.max)
        capability[ids.constrained] <- sample(
            capabilities.nontarget, length(ids.constrained), replace=TRUE
        )
        capability[target.group] <- round(
            runif(length(target.group), min=45, max=55)
        )
    } else {
        # Assign constraint values
        ids.constrained <- sample(ids, percent.constrained * number.nodes)
        service = rep(service.max, number.nodes)
        service[ids.constrained] <- round(
            runif(length(ids.constrained), min=1, max=service.max)
        )
        capability = rep(capability.max, number.nodes)
        capability[ids.constrained] <- round(
            runif(length(ids.constrained), min=1, max=capability.max)
        )
    }

    return(list(service, capability))
}

# Define the various node classes
#
# Author: Cody Lewis
# Date: 2019-05-01

source("Report.r")

# Some calculation flags
NORMAL <- 0
N <- 1  # Note based mitigation
C <- 2  # Context based mitigation
CN <- 3  # Context and note based mitigation, (became redundant)
MC <- 4  # Mean context based mitigation
R <- 5  # Replay based mitigation
LOCAL <- 0
GLOBAL <- 1

# A generic node class
Node <- setRefClass(
    "Node",

    fields=list(
        id="numeric",
        service="numeric",
        capability="numeric",
        noteacc="numeric",
        QR="numeric",
        malicious="logical",
        time.QR="numeric",
        reports="list",
        reputation="numeric",
        trust="numeric",
        type.calc="list",
        time.possible.attack="numeric",
        time.disregard="numeric",
        avg.capability="numeric",
        avg.service="numeric",
        number.reports="numeric"
    ),

    methods=list(
        initialize = function(id, service, capability, noteacc, QR, malicious,
                              number.nodes, type.calc, time.disregard=1) {
            id <<- id
            service <<- service
            capability <<- capability
            noteacc <<- noteacc
            QR <<- QR
            malicious <<- malicious
            time.QR <<- 0
            type.calc <<- type.calc
            time.disregard <<- time.disregard
            if(type.calc[[2]] >= N) {
                time.possible.attack <<- rep(-time.disregard - 1, number.nodes)
            }
            if(type.calc[[2]] == MC) {
                avg.capability <<- -1
                avg.service <<- -1
                number.reports <<- 0
            }
        },

        take.note = function(target.service, target.capability, proxy, time) {
            "Take note of the Quality of the service provided by a proxy"
            note = find.note(target.service, target.capability, proxy, time)
            if(runif(1) > noteacc) {
                wrong_vals = setdiff(c(-1, 0, 1), note)
                return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
            }
            return(note)
        },

        take.service = function(target.service) {
            "Get the service value for a report"
            return(target.service)
        },

        take.capability = function(proxy) {
            "Get the capability value for a report"
            return(proxy$capability)
        },

        take.time = function(time) {
            "Get the time value for a report"
            return(time)
        },

        make.report = function(proxy, target.service, target.capability,
                               time) {
            "Create a report on the proxy server"
            note = take.note(target.service, target.capability, proxy, time)
            id.attacker = `if`(proxy$type.calc[[1]] == GLOBAL, proxy$id, id)
            report.service = take.service(target.service)
            report.capability = take.capability(proxy)
            report.time = take.time(time)

            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=report.service,
                capability=report.capability,
                time=report.time,
                note=note,
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]],
                disregard=proxy$calc.disregard(
                    id.attacker, report.capability, report.service, note,
                    report.time
                )
            )
            if((proxy$type.calc[[2]] %in% c(N, C, CN)) && note == -1) {
                proxy$time.possible.attack[[id.attacker]] <- time
            }

            if(proxy$type.calc[[2]] == MC) {
                proxy$avg.capability <- proxy$avg.capability *
                    proxy$number.reports + report.capability
                proxy$avg.service <- proxy$avg.service * proxy$number.reports +
                    report.service
                proxy$number.reports <- proxy$number.reports + 1
                proxy$avg.capability <- proxy$avg.capability /
                    proxy$number.reports
                proxy$avg.service <- proxy$avg.service / proxy$number.reports
            }
        },

        calc.disregard = function(id.attacker, capability, service, note,
                                  time) {
            "Figure out whether the current report should be disregarded"
            if(type.calc[[2]] %in% c(N, C, CN)) {
               return(
                    !is.na(time.possible.attack[[id.attacker]]) &&
                    time.possible.attack[[id.attacker]] >=
                    time - time.disregard &&
                    `if`(type.calc[[2]] %in% c(N, CN), note == -1, TRUE) &&
                    `if`(
                        type.calc[[2]] %in% c(C, CN),
                        check.context(capability, service, note),
                        TRUE
                    )
               )
            } else if(type.calc[[2]] == R) {
                return(
                    !is.na(time.possible.attack[[id.attacker]]) &&
                    time.possible.attack[[id.attacker]] >=
                    time - time.disregard
               )
            } else if(type.calc[[2]] == MC) {
                fuzz = 1
                condition = (avg.capability - fuzz <= capability &&
                             capability <= avg.capability + fuzz) ||
                    (avg.service - fuzz <= service &&
                     service <= avg.service + fuzz)
                return(condition)
            }

            return(FALSE)
        },

        check.context = function(reported.capability, reported.service,
                                 reported.note) {
            "Check for a context attacks in a lattice fashion"
            return(
                reported.capability <= capability &&
                    reported.service <= service &&
                    reported.note == -1
            )
        }
    )
)

# A Bad mouthing node
Node.BadMouther <- setRefClass(
    "Node.BadMouther",
    contains="Node",
    methods=list(
        take.note = function(target.service, target.capability, proxy, time) {
            "Take a bad mouthing note, -1"
            return(-1)
        }
    )
)

# A service setting node, this always reports the service as 50
Node.BadMouther.ServiceSetter <- setRefClass(
    "Node.BadMouther.ServiceSetter",
    contains="Node.BadMouther",
    methods=list(
        take.service = function(target.service) {
            "Give a service setted service value"
            return(context.set())
        }
    )
)

# A capability setting node, this always reports the capability as 50
Node.BadMouther.CapabilitySetter <- setRefClass(
    "Node.BadMouther.CapabilitySetter",
    contains="Node.BadMouther",
    methods=list(
        take.capability = function(proxy) {
            "Give a capability setted capability value"
            return(context.set())
        }
    )
)

# A service and capability setting node
Node.BadMouther.CapabilitySetter.ServiceSetter <- setRefClass(
    "Node.BadMouther.CapabilitySetter.ServiceSetter",
    contains="Node.BadMouther.CapabilitySetter",
    methods=list(
        take.service = function(target.service) {
            "Give a service setted service value"
            return(context.set())
        }
    )
)

# A time decaying node, this always reports the time as 5 units in the past
Node.BadMouther.TimeDecayer <- setRefClass(
    "Node.BadMouther.TimeDecayer",
    contains="Node.BadMouther",
    methods=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A capability setting and time decaying node
Node.BadMouther.CapabilitySetter.TimeDecayer <- setRefClass(
    "Node.BadMouther.CapabilitySetter.TimeDecayer",
    contains="Node.BadMouther.CapabilitySetter",
    methods=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A service setting and time decaying node
Node.BadMouther.ServiceSetter.TimeDecayer <- setRefClass(
    "Node.BadMouther.ServiceSetter.TimeDecayer",
    contains="Node.BadMouther.ServiceSetter",
    methods=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A capability setting, service setting and time decaying node
Node.BadMouther.CapabilitySetter.ServiceSetter.TimeDecayer <- setRefClass(
    "Node.BadMouther.CapabilitySetter.ServiceSetter.TimeDecayer",
    contains="Node.BadMouther.CapabilitySetter.ServiceSetter",
    methods=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# Find the suitable note, by comparing the target context to the proxy's
# abilties
find.note <- function(target.service, target.capability, proxy, time) {
    if(proxy$malicious) {
        return(-1)
    } else if(proxy$service >= target.service &&
              proxy$capability >= target.capability) {
        return(1)
    } else {
        return(0)
    }
}

# give a setted context
context.set <- function() {
    return(50)
}

# decay the input time
time.decay <- function(time) {
    return(time - 5)
}

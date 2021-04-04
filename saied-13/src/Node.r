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
Node <- R6::R6Class(
    "Node",
    list(
        id= 0,
        service = 0,
        capability = 0,
        noteacc = 0,
        QR = 0,
        malicious = 0,
        time.QR = 0,
        reports = list(),
        reputation = 0,
        trust = 0,
        type.calc = list(),
        time.possible.attack = 0,
        time.disregard = 0,
        avg.capability = 0,
        avg.service = 0,
        number.reports = 0,

        initialize = function(id, service, capability, noteacc, QR, malicious,
                              number.nodes, type.calc, time.disregard=1) {
            self$id <- id
            self$service <- service
            self$capability <- capability
            self$noteacc <- noteacc
            self$QR <- QR
            self$malicious <- malicious
            self$time.QR <- 0
            self$type.calc <- type.calc
            self$time.disregard <- time.disregard
            if(type.calc[[2]] >= N) {
                self$time.possible.attack <- rep(-time.disregard - 1, number.nodes)
            }
            if(type.calc[[2]] == MC) {
                self$avg.capability <- -1
                self$avg.service <- -1
                self$number.reports <- 0
            }
        },

        take.note = function(target.service, target.capability, proxy, time) {
            "Take note of the Quality of the service provided by a proxy"
            note = find.note(target.service, target.capability, proxy, time)
            if(runif(1) > self$noteacc) {
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
            note = self$take.note(target.service, target.capability, proxy, time)
            id.attacker = `if`(proxy$type.calc[[1]] == GLOBAL, proxy$id, self$id)
            report.service = self$take.service(target.service)
            report.capability = self$take.capability(proxy)
            report.time = self$take.time(time)
            proxy$reports[[length(proxy$reports) + 1]] <- Report$new(
                service=report.service,
                capability=report.capability,
                time=report.time,
                note=note,
                issuer=self$id,
                issuer.QR=self$QR[[1]],
                issuer.time.QR=self$time.QR[[1]],
                disregard=proxy$calc.disregard(
                    id.attacker, report.capability, report.service, note,
                    report.time
                )
            )
            if((proxy$type.calc[[2]] %in% c(N, C, CN)) && note == -1) {
                proxy$time.possible.attack[[id.attacker]] <- time
                print(proxy$time.possible.attack[[id.attacker]])
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
            if(self$type.calc[[2]] %in% c(N, C, CN)) {
               return(
                    !is.na(time.possible.attack[[id.attacker]]) &&
                    self$time.possible.attack[[id.attacker]] >=
                    time - self$time.disregard &&
                    `if`(self$type.calc[[2]] %in% c(N, CN), note == -1, TRUE) &&
                    `if`(
                        self$type.calc[[2]] %in% c(C, CN),
                        self$check.context(capability, service, note),
                        TRUE
                    )
               )
            } else if(self$type.calc[[2]] == R) {
                return(
                    !is.na(self$time.possible.attack[[id.attacker]]) &&
                    self$time.possible.attack[[id.attacker]] >=
                    time - self$time.disregard
               )
            } else if(self$type.calc[[2]] == MC) {
                fuzz = 1
                condition = (self$avg.capability - fuzz <= capability &&
                             capability <= self$avg.capability + fuzz) ||
                    (self$avg.service - fuzz <= service &&
                     service <= self$avg.service + fuzz)
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
Node.BadMouther <- R6::R6Class(
    "Node.BadMouther",
    inherit=Node,
    public=list(
        take.note = function(target.service, target.capability, proxy, time) {
            "Take a bad mouthing note, -1"
            return(-1)
        }
    )
)

Node.ContextAttacker <- R6::R6Class(
        "Node.ContextAttacker",
        inherit=Node,
        public=list(
            take.note = function(target.service, target.capability, proxy, time) {
                    "Take note of the Quality of the service provided by a proxy"
                    attack.val <- context.set()
                    tau <- 0.05
                    if(abs(target.service - attack.val) < tau && abs(target.capability - attack.val) < tau) {
                        return(-1)
                    }
                    note = find.note(target.service, target.capability, proxy, time)
                    if(runif(1) > self$noteacc) {
                        wrong_vals = setdiff(c(-1, 0, 1), note)
                        return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
                    }
                    return(note)
            }
        )
)

# A service setting node, this always reports the service as 50
Node.BadMouther.ServiceSetter <- R6::R6Class(
    "Node.BadMouther.ServiceSetter",
    inherit=Node.BadMouther,
    public=list(
        take.service = function(target.service) {
            "Give a service setted service value"
            return(context.set())
        }
    )
)

# A capability setting node, this always reports the capability as 50
Node.BadMouther.CapabilitySetter <- R6::R6Class(
    "Node.BadMouther.CapabilitySetter",
    inherit=Node.BadMouther,
    public=list(
        take.capability = function(proxy) {
            "Give a capability setted capability value"
            return(context.set())
        }
    )
)

# A service and capability setting node
Node.BadMouther.CapabilitySetter.ServiceSetter <- R6::R6Class(
    "Node.BadMouther.CapabilitySetter.ServiceSetter",
    inherit=Node.BadMouther.CapabilitySetter,
    public=list(
        take.service = function(target.service) {
            "Give a service setted service value"
            return(context.set())
        }
    )
)

# A time decaying node, this always reports the time as 5 units in the past
Node.BadMouther.TimeDecayer <- R6::R6Class(
    "Node.BadMouther.TimeDecayer",
    inherit=Node.BadMouther,
    public=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A capability setting and time decaying node
Node.BadMouther.CapabilitySetter.TimeDecayer <- R6::R6Class(
    "Node.BadMouther.CapabilitySetter.TimeDecayer",
    inherit=Node.BadMouther.CapabilitySetter,
    public=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A service setting and time decaying node
Node.BadMouther.ServiceSetter.TimeDecayer <- R6::R6Class(
    "Node.BadMouther.ServiceSetter.TimeDecayer",
    inherit=Node.BadMouther.ServiceSetter,
    public=list(
        take.time = function(time) {
            "Give a time decayed time value"
            return(time.decay(time))
        }
    )
)

# A capability setting, service setting and time decaying node
Node.BadMouther.CapabilitySetter.ServiceSetter.TimeDecayer <- R6::R6Class(
    "Node.BadMouther.CapabilitySetter.ServiceSetter.TimeDecayer",
    inherit=Node.BadMouther.CapabilitySetter.ServiceSetter,
    public=list(
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

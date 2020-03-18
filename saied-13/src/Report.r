#!/usr/bin/env Rscript

# A reference class for a report
#
# Author: Cody Lewis
# Date: 2019-05-01

Report <- setRefClass(
    "Report",
    fields=list(
        service="numeric",
        capability="numeric",
        time="numeric",
        note="numeric",
        issuer="numeric",
        issuer.QR="numeric",
        issuer.time.QR="numeric",
        server="logical",
        disregard="logical"
    ),
    methods=list(
        initialize = function(service, capability, time, note, issuer,
                              issuer.QR, issuer.time.QR, disregard=FALSE) {
            service <<- service
            capability <<- capability
            time <<- time
            note <<- note
            issuer <<- issuer
            issuer.QR <<- issuer.QR
            issuer.time.QR <<- issuer.time.QR
            server <<- FALSE
            disregard <<- disregard
        }
    )
)

# Find the one dimensional distance between the target and current
find.diff <- function(target, current) {
    return(abs(target - current))
}

# Find the distance of a report from the target
report.distance <- function(report, target.service, target.capability,
                            service.max, capability.max, eta) {
    service.max = service.max + 1
    capability.max = capability.max + 1
    dS.max.squared = find.diff(target.service, service.max)**2
    dC.max.squared = find.diff(target.capability, capability.max)**2
    term.shared = sqrt(
        (dS.max.squared + dC.max.squared) *
            (
                (
                    find.diff(target.service, report$service)**2 /
                        dS.max.squared
                ) +
                (
                    find.diff(target.capability, report$capability)**2 /
                        dC.max.squared
                )
            )
    )
    if(report$note >= 0) {
        term.unique = sqrt(
            (dS.max.squared + dC.max.squared) *
                (
                    (
                        (service.max - report$service) /
                            (service.max - (target.service - eta))
                    )**2 +
                    (
                        report$capability / (target.capability + eta)
                    )**2
                )
        )
    } else {
        term.unique = sqrt(
            (dS.max.squared + dC.max.squared) *
                (
                    (
                        (capability.max - report$capability) /
                            (capability.max - (target.capability - eta))
                    )**2 +
                    (report$service / (target.service + eta))**2
                )
        )
    }
    return(min(term.shared, term.shared))
}

# Return a weight for a report, stating its relevance
report.weigh <- function(report, dist, lambda, theta, time.current) {
    s = (1 / 2) * (report$note**2 - report$note)
    return(lambda**dist * theta**((s + 1) * (time.current - report$time)))
}

# Find the threshold value that states whether a report should be rejected if
# exceeded
find.t <- function(target.service, target.capability, service.max,
                   capability.max) {
    return(
        sqrt(
            find.diff(target.service, service.max)**2 +
                find.diff(target.capability, capability.max)**2
        )
    )
}

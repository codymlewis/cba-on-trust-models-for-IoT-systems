#!/usr/bin/env Rscript

# Unit tests for the program
#
# Author: Cody Lewis
# Date: 2019-05-02

library('testthat')

source("Report.r")
source("Node.r")

# Test the report functionality
test.report <- function() {
    r = Report(service=50, capability=50, time=0, note=1, issuer=1,
               issuer.QR=1, issuer.time.QR=0)
    report.check(r, 50, 50, 0, 1, 1, 1, 0)
    d = report.distance(r, 100, 100, 100, 100, 1)
    expect_that(d, equals(100))
    w = report.weigh(r, d, 0.7, 0.7, 0)
    expect_that(w, equals(3.234477e-16))
}

# Check that the values stored in a report match what is expected
report.check <- function(report, service, capability, time, note, issuer,
                         issuer.QR, issuer.time.QR) {
    expect_that(report$service, equals(service))
    expect_that(report$capability, equals(capability))
    expect_that(report$time, equals(time))
    expect_that(report$note, equals(note))
    expect_that(report$issuer, equals(issuer))
    expect_that(report$issuer.QR, equals(issuer.QR))
    expect_that(report$issuer.time.QR, equals(issuer.time.QR))
}

# Test the node functionality
test.node <- function() {
    number.nodes = 3
    type.calc = list(NORMAL, NORMAL)
    n <- Node(id=1, service=1, capability=1, noteacc=1, QR=1, malicious=F,
              number.nodes, type.calc)
    node.check(n, 1, 1, 1, 1, 1, F)
    n1 <- Node(id=2, service=100, capability=100, noteacc=1, QR=1, malicious=T,
               number.nodes, type.calc)
    node.check(n1, 2, 100, 100, 1, 1, T)
    n2 <- Node(id=3, service=100, capability=1, noteacc=1, QR=1, malicious=F,
               number.nodes, type.calc)
    node.check(n2, 3, 100, 1, 1, 1, F)
    n$make.report(n1, 50, 50, 0)
    report.check(n1$reports[[1]], 50, 100, 0, -1, 1, 1, 0)
    n1$make.report(n, 50, 50, 0)
    report.check(n$reports[[1]], 50, 1, 0, 0, 2, 1, 0)
    n$make.report(n2, 50, 1, 0)
    report.check(n2$reports[[1]], 50, 1, 0, 1, 1, 1, 0)
}

# Check that the values stored in a node a what is expected
node.check <- function(node, id, service, capability, noteacc, QR, malicious) {
    expect_that(node$id, equals(id))
    expect_that(node$service, equals(service))
    expect_that(node$capability, equals(capability))
    expect_that(node$noteacc, equals(noteacc))
    expect_that(node$QR, equals(QR))
    expect_that(node$malicious, equals(malicious))
}

test <- function() {
    cat("-------------- Performing Tests --------------\n")
    test.report()
    cat(".")
    test.node()
    cat(".")
    cat("\n")
    cat("------------- Test Cases Passed --------------\n")
    cat("Build Succeeded\n")
}

test()

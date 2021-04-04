#!/usr/bin/env -S Rscript --vanilla

# The main console interface fo the trust model
#
# Author: Cody Lewis
# Date: 2019-05-03

library("optparse")

source("TrustModel.r")
source("Functions.r")

# Find the malicious typing specified in the arguments
find.malicious.type <- function(opt) {
    malicious.type = ""
    if(opt$context_attack) {
        return("ca")
    }
    if(opt$bad_mouth) {
        malicious.type = paste(malicious.type, "bm", sep="")
    } else if(opt$good_mouth) {
        malicious.type = paste(malicious.type, "gm", sep="")
    } else if(opt$on_off) {
        malicious.type = paste(malicious.type, "oo", sep="")
    }
    if(opt$service_set) {
        malicious.type = paste(malicious.type, "ss", sep="")
    }
    if(opt$capability_set) {
        malicious.type = paste(malicious.type, "cs", sep="")
    }
    if(opt$time_decay) {
        malicious.type = paste(malicious.type, "td", sep="")
    }

    return(malicious.type)
}

# Give a list detailing the attack types to perform based on the input string
parse.type.calc.string <- function(type.calc.string) {
    if(type.calc.string == "normal") {
        return(list(LOCAL, NORMAL, FALSE, FALSE, FALSE, FALSE))
    } else if(type.calc.string == "mitigating") {
        return(list(LOCAL, NORMAL, FALSE, FALSE, FALSE, TRUE))
    }
    if(grepl("n", type.calc.string)) {
        if(grepl("c", type.calc.string)) {
            type.detection = CN
        } else {
            type.detection = N
        }
    } else if(grepl("mc", type.calc.string)) {
        type.detection = MC
    } else if(grepl("c", type.calc.string)) {
        type.detection = C
    } else if(grepl("r", type.calc.string)) {
        type.detection = R
    } else {
        type.detection = NORMAL
    }
    return(
        list(
            `if`(grepl("g", type.calc.string), GLOBAL, LOCAL),
            type.detection,
            grepl("a", type.calc.string), # disregard causes note negation
            grepl("s", type.calc.string), # Split calculations
            grepl("q", type.calc.string), # disregard also punishes QR
            grepl("mit", type.calc.string) # perform a mitigating calculation
        )
    )
}

# The main program flow
main <- function() {
    option_list <- list(
        make_option(c("--bad_mouth"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the bad mouthing attack"),
        make_option(c("--good_mouth"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the good mouthing attack"),
        make_option(c("--on_off"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the on-off attack"),
        make_option(c("--service_set"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the service setting attack (along with mouthing, they always report a particular service value)"),
        make_option(c("--capability_set"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the capability setting attack (along with mouthing, they always report a particular capability value)"),
        make_option(c("--time_decay"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the time decay attack (along with mouthing, they always report a reduced time value)"),
        make_option(c("--context_attack"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the context attack"),
        make_option(c("--theta", "-t"), type="double", default=0.7,
                    help="Value for theta [default %default]"),
        make_option(c("--lambda", "-l"), type="double", default=0.7,
                    help="Value for lambda [default %default]"),
        make_option(c("--eta", "-e"), type="integer", default=1,
                    help="Value for eta [default %default]"),
        make_option(c("--total_nodes"), type="integer", default=200,
                    help="Number of nodes in the simulated network [default %default]"),
        make_option(c("--transactions"), type="integer", default=300,
                    help="Number of transactions to simulate [default %default]"),
        make_option(c("--poor_witnesses", "-p"), type="double", default=0.2,
                    help="Percentage of poor witnesses in decimal form [default %default]"),
        make_option(c("--constrained", "-c"), type="double", default=0.5,
                    help="Percentage of constrained nodes in decimal form [default %default]"),
        make_option(c("--malicious"), type="double", default=0.1,
                    help="Percentage of malicious nodes to start with [default %default]"),
        make_option(c("--malicious_start"), type="double", default=0,
                    help="Percentage of malicious reporting nodes to start with [default %default]"),
        make_option(c("--malicious_end"), type="double", default=1,
                    help="Percentage of malicious reporting nodes to end with [default %default]"),
        make_option(c("--malicious_jump"), type="double", default=0.05,
                    help="Percentage of malicious reporting nodes to increment by [default %default]"),
        make_option(c("--reputation", "-r"), type="double", default=-1,
                    help="Reputation threshold, nodes in the network that fall below this are no longer considered in the network"),
        make_option(c("--targeted"), action="store_true", default=FALSE,
                    help="Analyze the targeted effects of an attack"),
        make_option(c("--type_calc"), type="character", default="normal",
                    help="Assign a type of calculation for report relevance the first letter states local or global (l, g), and the second states whether to detect based on notes or notes and context (n, c) [default %default]"),
        make_option(c("--time_change"), type="integer",
                    action="store", default=60,
                    help="The number epochs to increment the time at. [default %default]"),
        make_option(c("--disregard_multiplier"), action="store",
                    type="double", default=1,
                    help="Amount to multiply disregarded reports effects on QR [default %default]")
    )
    parser <- OptionParser(usage="%prog [options]", option_list=option_list)
    args <- parse_args(parser, positional_arguments=0)
    opt <- args$options

    type.malicious = find.malicious.type(opt)
    type.calc = parse.type.calc.string(opt$type_calc)

    dir.create("./graphs", showWarnings=FALSE)
    status.alt = opt$type_calc
    dir.create(
        sprintf("./graphs/%d", opt$total_nodes), showWarnings=FALSE
    )
    dir.create(
        sprintf("./graphs/%d/%s", opt$total_nodes, status.alt),
        showWarnings=FALSE
    )
    dir.create(
        sprintf(
            "./graphs/%d/%s/%f", opt$total_nodes, status.alt, opt$reputation
        ),
        showWarnings=FALSE
    )
    dir.create(
        sprintf(
            "./graphs/%d/%s/%f/%s",
            opt$total_nodes,
            status.alt,
            opt$reputation,
            type.malicious
        ),
        showWarnings=FALSE
    )
    malicious.increments = seq(
        opt$malicious_start, opt$malicious_end, by=opt$malicious_jump
    )
    for(percent.malicious.reporters in malicious.increments) {
        tm <- TrustManager$new(
            eta=opt$eta,
            lambda=opt$lambda,
            theta=opt$theta,
            service.max=100,
            capability.max=100,
            reputation.threshold=opt$reputation,
            QR.initial=1
        )
        tm$init(
            number.nodes=opt$total_nodes,
            percent.constrained=opt$constrained,
            percent.poorwitness=opt$poor_witnesses,
            percent.malicious=opt$malicious,
            percent.malicious.reporter=percent.malicious.reporters,
            type.malicious=type.malicious,
            targeted=opt$targeted,
            type.calc=type.calc,
            disregard.multiplier=opt$disregard_multiplier
        )
        epochs.total <- opt$transactions
        time.current <- 0
        cat(
            sprintf(
                "Performing %d transactions in the network, with %f%% %s with calculation %s\n",
                epochs.total,
                percent.malicious.reporters * 100,
                type.malicious,
                status.alt
            )
        )
        for(epoch in 1:epochs.total) {
            if((epoch %% opt$time_change) == 0) {
                time.current <- time.current + 1
            }
            tm$phase(opt$total_nodes * 5, time.current)
            cat.progress(
                epoch,
                epochs.total,
                prefix=sprintf("%d/%d epochs completed", epoch, epochs.total)
            )
        }
        loc.save = sprintf(
            "total_nodes=%d/calc_type=%s/dis_mul=%f/rep_threshold=%f/mal_type=%s/mal_rep_per=%f",
            opt$total_nodes,
            status.alt,
            opt$disregard_multiplier,
            opt$reputation,
            type.malicious,
            percent.malicious.reporters * 100
        )
        dir.create(
            sprintf("./graphs/%s", loc.save),
            recursive=TRUE,
            showWarnings=FALSE
        )
        plot.nodes(
            c(
                tm$nodes[[tm$id.nodemon.normal]],
                tm$nodes[[tm$id.nodemon.malicious]]
            )
        )
        graph.save(sprintf("%s/qr_changes.png", loc.save))
        plot.trust(tm$nodes)
        graph.save(sprintf("%s/trust.png", loc.save))
        plot.QRs.final(tm$nodes)
        graph.save(sprintf("%s/final_qrs.png", loc.save))
        status.plt = `if`(
            opt$targeted,
            plot.trust.targeted(tm$nodes, epochs.total),
            plot.trust.mean(tm$nodes, epochs.total)
        )
        if(!is.na(status.plt)) {
            graph.save(sprintf("%s/targeted_trust.png", loc.save))
        }
        node.group.samples = c(
            sample(1:floor(length(tm$nodes) / 6.6), 5),
            sample(ceiling(length(tm$nodes) / 6.6):length(tm$nodes), 5)
        )
        for(id.node in node.group.samples) {
            plot.node.trust(tm$nodes[[id.node]], length(tm$nodes))
            graph.save(sprintf("%s/node_%d_trust.png", loc.save, id.node))
        }
        rm(tm)
    }
}

main()

#!/usr/bin/env Rscript
# Author: Cody Lewis
# Description:
# Web based GUI interface showing how the reputation equation works

library(shiny)

source("ReputationQR.r")

NORMAL_FLAG = 0
BAD_MOUTH_FLAG = 1
GOOD_MOUTH_FLAG = 2

# The GUI instantiation
ui <- fluidPage(
    titlePanel("Reputation Impacts in a Trust Model"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId="s_target",
                label="Service Target:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="c_target",
                label="Capability Target:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="s_j",
                label="Reported Service of the Node:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="c_j",
                label="Reported Capability of the Node:",
                min=1,
                max=100,
                value=50
            ),
            radioButtons(
                inputId="reporter_attack_type",
                label="Note Taking Type of Reporting Nodes:",
                choices=list(
                    "Normal"=NORMAL_FLAG,
                    "Bad Mouth"=BAD_MOUTH_FLAG,
                    "Good Mouth"=GOOD_MOUTH_FLAG
                ),
                selected=NORMAL_FLAG
            ),
            sliderInput(
                inputId="time_decay",
                label="Time Decay of Reports:",
                min=0,
                max=10,
                value=0
            ),
            sliderInput(
                inputId="reporter_qr_range",
                label="Range of QRs of the reporters:",
                min=0,
                max=1,
                value=c(0, 1)
            ),
            sliderInput(
                inputId="theta",
                label="θ:",
                min=0,
                max=1,
                value=0.7
            ),
            sliderInput(
                inputId="lambda",
                label="λ:",
                min=0,
                max=1,
                value=0.7
            ),
            sliderInput(
                inputId="eta",
                label="η:",
                min=0,
                max=20,
                value=1
            ),
            sliderInput(
                inputId="transactions",
                label="Transactions:",
                min=1,
                max=1000,
                value=300
            )
        ),
        mainPanel(
            id="main",
            plotOutput("reputation"),
            textOutput("context_attack_impact")
        )
    )
)

# Backend logic
server <- function(input, output) {
    # Plot the effects of the attack on reputation
    output$reputation <- renderPlot({
        # initialize for the transactions
        reputations = rep(1, each=(input$transactions + 1))
        node_qrs = c(1)
        node_qr_times = c(1)
        time = 1
        client_qrs = runif(
            input$transactions,
            min=input$reporter_qr_range[[1]],
            max=input$reporter_qr_range[[2]]
        )
        if(input$reporter_attack_type == NORMAL_FLAG) {
            client_notes = take_notes(input$c_j, input$c_target,
                                      input$s_j, input$s_target,
                                      client_qrs)
        } else if(input$reporter_attack_type == BAD_MOUTH_FLAG) {
            client_notes = rep(-1, each=length(client_qrs))
        } else {
            client_notes = rep(1, each=length(client_qrs))
        }
        node_note = 1
        # Perform the transactions
        for(transaction in 1:input$transactions) {
            if((transaction %% 30) == 0) {
                time = time + 1
            }
            d = report_dist(
                input$c_j, input$s_j, input$c_target,
                input$s_target, input$eta, node_note,
                find_dist(input$s_target, S_MAX)**2,
                find_dist(input$c_target, C_MAX)**2,
                S_MAX, C_MAX
            )
            w = find_weight(input$lambda, input$theta, node_note,
                            (time - input$time_decay), d, time)
            qr = find_qr(
                w, client_notes[[transaction]], input$theta, time,
                node_note, client_qrs[[transaction]], node_qrs, node_qr_times
            )
            node_qrs = c(qr, node_qrs)
            node_qr_times = c(time, node_qr_times)
            reputations[[transaction + 1]] = calculate_reputation(
                input$theta, client_notes, client_qrs, transaction,
                time, node_qr_times
            )
        }
        plot_reputation(reputations)
    })
    # State whether the attack will have an impact on QR of the client
    output$context_attack_impact <- renderText({
        time = 1
        node_note = 1
        dS_max_sq = find_dist(input$s_target, S_MAX)**2
        dC_max_sq = find_dist(input$c_target, C_MAX)**2
        d = report_dist(
            input$c_j, input$s_j, input$c_target,
            input$s_target, input$eta, node_note,
            dS_max_sq, dC_max_sq, S_MAX, C_MAX
        )
        if(d >= sqrt(dS_max_sq + dC_max_sq)) {
            return("Reports produced in the transaction phase of this attack will have no impact on the client's QR as the distance of the report is out of bounds")
        } else {
            w = find_weight(input$lambda, input$theta, node_note,
                            (time - input$time_decay), d, time)
            return(
                sprintf("Reports produced in the transaction phase of this attack will have a weight of %f", w)
            )
        }
    })
}

options(shiny.port=8100)
shinyApp(ui=ui, server=server)

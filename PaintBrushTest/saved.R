#PaintBrushTest
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Linear Regression",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
             column(width=4,
                    selectInput("selstart","Starting Coin",choices = c("Ethereum", "Bitcoin", "Solana")),
                    h2("Please select an area by using your mouse as a paint brush over the graph, then Hit the button below to draw a regression line."),
                    actionButton("linearReg", "Draw Linear Regression"),
                    uiOutput("eqn")
             ),
             column(width=4,
                    uiOutput("title"),
                    box(width = NULL,
                        plotOutput("plot1", click = "plot_click", 
                                   brush = brushOpts(id = "plot_brush"),
                                   width = 600)
                    ),
                    # verbatimTextOutput("info"),
                    plotOutput("zoom",width = 600)
             )
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
    #Variables that are shared among server functions (use <<-)
    updateSelectInput(session, "selstart", choices = c("Ethereum", "Bitcoin", "Solana"))
    observeEvent(input$selstart, {
        
        eth <<- read.csv(paste0(input$selstart, ".csv"))
        # eth <<- cbind(eth,AbsoluteValue = abs(eth$Change_Percent))
        output$title <- renderUI(input$selstart)
        vals <- reactiveValues(mtc = eth[,c("Vol.", "Change_Percent")])
        output$plot1 <- renderPlot({plot(vals$mtc$Vol., vals$mtc$Change_Percent)})
    })
    eth <- read.csv("Ethereum.csv")
    vals <- reactiveValues(mtc = eth[,c("Vol.", "Change_Percent")])
    output$plot1 <- renderPlot({plot(vals$mtc$Vol., vals$mtc$Change_Percent)})
    output$zoom <- renderPlot({plot(vals$mtc$Vol., vals$mtc$Change_Percent,xlim = c(input$plot_brush$xmin,input$plot_brush$xmax),
                                    ylim = c(input$plot_brush$ymin,input$plot_brush$ymax))})
    #Initialization
    observeEvent(input$linearReg, {
        xmin <- input$plot_brush$xmin
        xmax <- input$plot_brush$xmax
        ymin <- input$plot_brush$ymin
        ymax <- input$plot_brush$ymax
        # indices <- which(vals$mtc$Vol.>=xmin & vals$mtc$Vol.<=xmax & vals$mtc$Change_Percent >= ymin & vals$mtc$Change_Percent >= ymax)
        # vals2 <- vals$mtc
        # indices <- which(vals2$Vol. >=xmin)
        # vals2 <- vals2[,indices]
        # indices <- which(vals2$Vol. <=xmax)
        # vals2 <- vals2[,indices]
        # indices <- which(vals2$Change_Percent >= ymin)
        # vals2 <- vals2[,indices]
        # indices <- which(vals2$Change_Percent <= ymax)
        rows <- brushedPoints(vals$mtc, input$plot_brush, xvar = "Vol.", yvar = "Change_Percent")
        
        predictor <- "Change_Percent"
        #Find the regression line by projection
        A <- cbind(rep(1,nrow(rows)),rows[,predictor])
        B <- t(A)%*%A
        P <- A%*%solve(B)%*%t(A)
        y.hat <- P%*%rows$Vol.    #predicted values, on the regression line
        output$zoom <- renderPlot({plot(rows[,predictor],rows$Vol.,xlab = "Volume", ylab = "Change Percent",pch = ".",cex = 3)  #scatter plot of the data
            points(rows[,predictor],y.hat,type = "b")
            
        })
        #Functions that respond to events in the input
    })
}

#Run the app
shinyApp(ui = ui, server = server)
#CryptoBetaFitter
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stats4)  #for mle()
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
header <- dashboardHeader(title = "Cryptocurrency Beta Fitter",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      radioButtons("stat","Select a metric", choices =
                       # c("Batting Average" = "BA",
                       #   "On Base Percentage" = "OBP",
                       #   "Slugging Percentage" = " SP",
                       #   "OPS/2" = "OPS")),
                     c("logprice" = "logprice",
                       "High" = "high",
                       "Low" = "low",
                       "vol" = "vol")),
      actionBttn("btnbeta","Fit a beta distribution"),
      actionBttn("btnopt","Optimize by hand")
                   
    ),
    column(width = 4,
           tableOutput("tbl")
    ),
    column(width = 4,
           plotOutput("plot1", click = "plot_click", hover = "plot_hover"),
           uiOutput("smean"),
           uiOutput("svar"),
           uiOutput("ab"),
           uiOutput("bmean"),
           uiOutput("bvar"),
           plotOutput("plot2", width = "100%", brush = "plot_brush", click = "plot_click" , hover = "plot_hover")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available





server <- function(session, input, output) {
    #Variables that are shared among server functions (use <<-)
    horiz <- numeric(1) 
    hname <- ""
    alpha <- 1
    beta <- 1
    plotBeta <- FALSE
    plotApprox <- FALSE
    # BatStat <- read.csv("Bat2016.csv")
    eth <- read.csv("Ethereum.csv")
    convert.log.price <- function() {
      eth <<- cbind(eth, logprice=log(eth$Price))
    }
    convert.log.price()
    #Function that calculates the negative log-likelihood
    MLL<- function(alpha, beta) {
      horiz <- horiz/max(horiz+1)
      # print(dbeta(horiz,alpha,beta, log = TRUE))
      print(which.max(dbeta(horiz,alpha,beta)))
      
        return(-sum( log(dbeta(horiz,alpha,beta) )))
    }
    vMLL <- Vectorize(MLL, c("alpha","beta"))
    
    plotStat <- function() {
      hist(horiz, breaks = 30, main = NULL, xlab = hname, probability =TRUE)
      if (plotBeta)
          curve( dbeta(x, alpha, beta),from = 0, to = 1,col = "blue", xlab = hname, add = TRUE )
      if (plotApprox)
        curve( dbeta(x, alpha, beta),from = 0, to = 1,col = "red", xlab = hname, add = TRUE )
    }
    
    observeEvent(input$stat, {
        if (input$stat == "logprice") {
            horiz <<- eth$logprice
            hname <<- "logprice"
        }
      if (input$stat == "high") {
        horiz <<- eth$High
        hname <<- "High"
      }
      if (input$stat == "low") {
        horiz <<- eth$Low
        hname <<- "Low"
      }
      if (input$stat == "vol") {
        horiz <<- eth$Vol.
        hname <<- "Volume"
      }
      output$tbl <- renderTable(eth[,c("logprice","High","Low","Vol.")],digits = 3)
      output$plot1 <- renderPlot(plotStat())
        output$smean <- renderUI(h4("Sample Mean = ",round(mean(horiz),6)))
        output$svar <- renderUI(h4("Sample Variance = ",round(var(horiz),6)))
     })
    
    observeEvent(input$btnbeta,{
        
        results<-mle(MLL, start = list(alpha = 0.05, beta = 20)) #an initial guess is required
        alpha <<- results@coef[1] 
        beta <<- results@coef[2] 
        print(alpha)
        plotBeta <<- TRUE
        output$plot1 <- renderPlot(plotStat())
        output$ab <- renderUI(h4("alpha = ", round(alpha,2),"beta = ", round(beta,2)))
        output$bmean <- renderUI(h4("Beta Mean = ",round(alpha/(alpha+beta),6)))
        output$bvar <- renderUI(h4("Beta Variance = ",round(alpha*beta/((alpha+beta)^2*(alpha+beta+1)),6)))
    })
    
    observeEvent(input$btnopt,{
        xx <- seq(from = 1, to = 150, length.out = 100)
        y <- seq(from = 1, to = 250, length.out = 100)
        z <- outer(xx,y, "vMLL")
        output$plot2 <- renderPlot(contour(xx,y,z,nlevels = 20,labcex = 1.2))
        
    })
    
    observeEvent(input$plot_click,{
      alpha <<- input$plot_click$x
      beta <<- input$plot_click$y
      plotBeta <<- FALSE
      plotApprox <<- TRUE
      output$plot1 <- renderPlot(plotStat())
      output$ab <- renderUI(h4("alpha = ", round(alpha,2),"beta = ", round(beta,2)))
      output$bmean <- renderUI(h4("Beta Mean = ",round(alpha/(alpha+beta),6)))
      output$bvar <- renderUI(h4("Beta Variance = ",round(alpha*beta/((alpha+beta)^2*(alpha+beta+1)),6)))
      
    })
    
    observeEvent(input$plot_hover,{
      alpha <<- input$plot_hover$x
      beta <<- input$plot_hover$y
      output$ab <- renderUI(h4("alpha = ", round(alpha,2),"beta = ", round(beta,2)))
      output$bmean <- renderUI(h4("Beta Mean = ",round(alpha/(alpha+beta),6)))
      output$bvar <- renderUI(h4("Beta Variance = ",round(alpha*beta/((alpha+beta)^2*(alpha+beta+1)),6)))
      
    })

    
    observeEvent(input$plot_brush,{
        xmin <<- input$plot_brush$xmin
        xmax <<- input$plot_brush$xmax
        ymin <<- input$plot_brush$ymin
        ymax <<- input$plot_brush$ymax
        xx <- seq(from = xmin, to = xmax, length.out = 100)
        y <- seq(from = ymin, to = ymax, length.out = 100)
        z <- outer(xx,y, "vMLL")
        output$plot2 <- renderPlot(contour(xx,y,z,nlevels = 40))
        session$resetBrush("plot_brush")
        
    })
}    

    


    


#Run the app
shinyApp(ui = ui, server = server)
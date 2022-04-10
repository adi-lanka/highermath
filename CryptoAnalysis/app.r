#CryptoAnalysis  - Fourier 
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))


#The user interface
header <- dashboardHeader(title = "Cryptoanalysis",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(id="menu",
                                        menuItem("Solana", tabName = "sol"),
                                        menuItem("Bitcoin", tabName = "btc"),
                                        menuItem("Ethereum", tabName = "eth")
                            ),
                            sliderInput("nbasis","Basis Functions to Display", min = 0, max = 9, value = 0),
                            sliderInput("ndisplay","Coefficients to Display", min = 20, max = 100, value = 40),
                            actionButton("btncalc","Calculate Fourier Coefficients"),
                            sliderInput("nrecon","Components for reconstruction", min = 1, max = 100, value = 5)
                            
                            
)
body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "sol",
            h2("Historical Solana Prices")
    ),
    tabItem(tabName = "eth",
            h2("Historical Ethereum Prices")
    ),
    tabItem(tabName = "btc",
            h2("Historical Bitcoin Prices"),
    )
  ),
  # , brush = brushOpts(id = "plot_brush")
  fluidRow(plotOutput("dataplot")),
  fluidRow(plotOutput("coefplot")),
  fluidRow(stylesheet,
           column(width = 6,
                  tableOutput("tbl")  #display the data set
           ),
           column(width = 6,
                  
           )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

source("fcalc.R")

#Additional functions are OK here, but no variables
eth <- read.csv("Ethereum.csv")

server <- function(session, input, output) {
  havecoeff <- FALSE
  colors = c("red", "blue", "green", "orange", "purple")
  dset <- numeric(0)
  N <- 0
  coefA <- numeric(0)
  coefB <- numeric(0)
  coefabs <- numeric(0)
  datamean <- numeric(0)
  showData <- function() plot(1:N,dset,type = "l")
  observeEvent(input$menu, {
    if(input$menu == "btc") {
      btc <- read.csv("Bitcoin.csv")
      dset <<- as.numeric(btc$Open)
      print(btc$Date)
      # dset <<- as.numeric(sunspots)  #we just want a vector
      N <<- length(dset)
      
    }
    if(input$menu == "sol") {
      sol <- read.csv("Solana.csv")
      dset <<- as.numeric(sol$Open)
      print(sol$Date)
      # dset <<- as.numeric(AirPassengers)  #we just want a vector
      N <<- length(dset)
    }
    if(input$menu == "eth"){
      eth <<- read.csv("Ethereum.csv")
      dset <<- as.numeric(eth$Open)
      print(eth$Date)
      N <<- length(dset)
      # dset <<- read.csv("flu2016.csv")$influ
      # N <<- length(dset)
    }
    havecoeff <<- FALSE
    output$coefplot  <<- NULL
    output$dataplot <- renderPlot({showData()})
  })
  observeEvent(input$btncalc,{
    f <- fa.makeCoefficients(dset)
    datamean <<- as.numeric(f$mean)
    coefA <<- f$cos
    coefB <<- f$sin
    coefabs <<- f$abs
    havecoeff <<- TRUE
    output$coefplot <- renderPlot(barplot(f$abs[1:input$ndisplay],names.arg = 1:input$ndisplay ))
  })
  
  observeEvent(input$nrecon,{
   
    if (havecoeff == FALSE) return()
    recon <- fa.reconstruct(datamean, coefA, coefB, input$nrecon)
    # print(length(recon))
    # print(N)
    #recon from Fourier results in one point not having a y coord so add min(n, length(recon))
    output$dataplot <- renderPlot({showData()
      points(1:min(N, length(recon)),recon, type = "l", col = "red")})
  })
  # output$zoom <- renderPlot({plot(vals$mtc$wt, vals$mtc$mpg,xlim = c(input$plot_brush$xmin,input$plot_brush$xmax),
                          # ylim = c(input$plot_brush$ymin,input$plot_brush$ymax))})
  observeEvent(input$nbasis,{
    output$dataplot <- renderPlot({
      showData()
      n <- input$nbasis
      if (n == 0) return()
      dmean <- mean(dset)
      points(1:N,rep(dmean,N), type = "l")
      if (n == 1) return()
      for (i in 1:floor(n/2)){
        points(1:N,dmean+(dmean-min(dset))*myCos(i,N), type = "l", col = colors[i], lwd = 2)
      }
      if (n == 2) return()
      for (i in 1:floor((n-1)/2)){
        points(1:N,dmean+(dmean-min(dset))*mySin(i,N), type = "l", col = colors[i], lwd = 2, lty = 2)
      }
    })
  })
}

#Run the app
shinyApp(ui = ui, server = server)
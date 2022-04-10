#D4Biggs
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
#source("jaxmat.R")   #for displaying mathematics

#The user interface
header <- dashboardHeader(title = "D4 According to Biggs",
                          titleWidth = 700)
sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr","Apply r"),
  actionButton("btns","Apply s"),
  actionButton("btnt","Apply t"),
  actionButton("btnx","Apply x"),
  actionButton("btny","Apply y"),
  actionButton("btnz","Apply z"),
  actionButton("btnw","Apply w")
)
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("triangle", height = 300)
    ),
    column(
      width = 6,
      #      dataTableOutput("multable")
      tableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
source("d4calc.R")
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  #Variables that are shared among server functions
  D4DF <- D4.makeDataFrame()
  config <- "ABCD"
  #Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$triangle <- renderPlot(D4.showSquare(config))
  tbl <-outer(D4DF$name,D4DF$name,vD4.multiply,DF=D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name 
  #Use options to suppress the fancy controls
  #  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
  output$multable <- renderTable(tbl, rownames = TRUE)
  #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  
  observeEvent(input$btni,{
    config <<- D4.apply("i",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr,{
    config <<- D4.apply("r",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btns,{
    config <<- D4.apply("s",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnt,{
    config <<- D4.apply("t",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnx,{
    config <<- D4.apply("x",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btny,{
    config <<- D4.apply("y",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnz,{
    config <<- D4.apply("z",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnw,{
    config <<- D4.apply("w",config)
    output$triangle <- renderPlot(D4.showSquare(config))
  })
}

#Run the app
shinyApp(ui = ui, server = server)
#ShortTest
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

#The user interface
header <- dashboardHeader(title = "Short Test")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),  #enable onclick 
  tags$iframe(name = "picture", width = "230", height = "270"),
  fluidRow(
        column(width = 6, 
           h2("Short Test Image"),
           h3("Which piece can checkmate the black king? (Hint: there are 3 options and the pieceds name rhymes with bite)"),
           img (id = "chess", src="Game2.jpg", alt="Checkmate Black", usemap="#chessmap", width="145", height="126"),
           tags$map( name="chessmap",
                     
                     tags$area(id = "knight", target = "picture", shape="rect" ,coords="40,74,56,88" ,alt="Knight", href = "knight.jpg"),
                     tags$area(id = "bishop", target = "picture", shape="rect" ,coords="86,18,102,34" ,alt="Bishop", href = "bishop.jpg"),
                     tags$area(id = "sadknight", target = "picture", shape="rect" ,coords="71,48,87,61" ,alt="Knight", href = "sadknight.jpg")),
           textOutput("checkmate")
           
    )
  ),
  fluidRow(
    column(width = 6,
           plotOutput("plot1", click = "plot_click"),
           textOutput("coord")
           )
  ),
  fluidRow(
    column(width = 6,
           h2("What is the best cryptocurrency?"),
           textOutput("btc"),
           textOutput("eth"),
           textOutput("sol"))
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")

#Variables that are shared among server functions

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  output$checkmate <- renderText("Make a move!")
  onclick("knight",{output$checkmate <- renderText("You clicked on the correct piece to checkmate!")})
  onclick("bishop",{output$checkmate <- renderText("You clicked on a sad bishop, wrong move!")})
  onclick("sadknight",{output$checkmate <- renderText("You clicked on a sad knight, wrong move!")})
  onclick("chess",{output$checkmate <- renderText("You clicked on the chess board, pick a move please!")})
  
  vals <- reactiveValues(arrests = USArrests[,c("Assault","UrbanPop")])
  output$plot1 <- renderPlot({plot(vals$arrests$Assault, vals$arrest$UrbanPop)})
  output$coord <- renderText(paste0("x= ",input$plot_click$x,"y= ",input$plot_click$y))
  #Initialization
  output$btc <- renderText("Bitcoin")
  output$eth <- renderText("Ethereum")
  output$sol <- renderText("Solana")
  onclick("btc", {output$btc <- renderText("It is Bitcoin, thank you Satoshi Nakamoto!")})
  onclick("eth", {output$eth <- renderText("It is Ethereum, thank you Vitalik Buterin!")})
  onclick("sol", {output$sol <- renderText("It is Solana, thank you Anatoly Yakovenko!")})
  
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)
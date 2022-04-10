#ThreeQuestions
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    h4 {
      color: red;
      background-color: white;
      font-style: italic
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Chess Quiz",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=6,
      h3("For each question choose the correct checkmate"), 
      h4("Notation Key: [piece name][x if capture][column][row][=piece promotion]"),
      actionBttn("btnq1", "Puzzle #1"),
      actionBttn("btnq2", "Puzzle #2"),
      actionBttn("btnq3", "Puzzle #3"),
      actionBttn("btnqs", "Submit Quiz")
      # textInput("ans1", "ans", "solve")
    ),
    column(width=6,
      h2("Quiz Score (2 Attempts max)"),
      img(src="checkmate.jpg", width = "65%"),
      uiOutput("triesleft"),
      uiOutput("currScore")
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
  #Current stored answers
  quiz1 <- 0
  quiz2 <- 0
  quiz3 <- 0
  score <- 0
  tries <- 1
  #Initialization
  
# Functions that respond to events in the input
  observeEvent(input$btnq1, {
    showModal(modalDialog
              (title = "Checkmate in One, please choose a move:", id = "firstp", 
                easyClose = TRUE, 
                # h2("The first puzzle"),
                
                footer = tagList(
                  actionButton("submitq1", "Submit"),
                  img(src="Game1.png", width = "55%"),
                  radioButtons(inputId = "q1", label = "Move Choices",
                               choices = c("Qxb5+", "Bxb4+", "Nxc4+"))
                )))
  })
  
  observeEvent(input$btnq2, {
    showModal(modalDialog(
      title = "Checkmate in One, please choose a move:", id="secondp",
      easyClose = TRUE,
      footer = tagList(
        actionButton("submitq2", "Submit"),
        img(src="Game2.jpg", width = "55%"),
        radioButtons(inputId = "q2", label = "Move Choices",
                     choices = c("Ng6+", "Nxc6+", "Nd5+"))
      )
    ))
  })

  observeEvent(input$btnq3, {
    showModal(modalDialog(
      title = "Checkmate in One, please choose a move:", id="secondp",
      easyClose = TRUE,
      footer = tagList(
        actionButton("submitq3", "Submit"),
        img(src="Game3.jpg", width = "55%"),
        radioButtons(inputId = "q3", label = "Move Choices",
                     choices = c("cxb8=N+", "cxd8=Q+", "Rxd6+"))
      )
    ))
  })
  
  observeEvent(input$btnqs, {
    if(tries >= 0) {
      
      score <<- quiz1 + quiz2 + quiz3
      
      output$triesleft <- renderUI(paste("Attempts left: ", tries+1))
      tries <<- tries - 1
      output$currScore <- renderUI(paste("Final score: ", score, "out of 3!"))
    }
    else {
      output$triesleft <- renderUI(paste("Nice try!"))
      output$currScore <- renderUI(paste("Too many attempts!"))
    }
    
  })
  
  observeEvent(input$submitq1, {
    if(input$q1 == "Nxc4+") {
      quiz1 <<- 1
      # output$answers <- renderUI(paste("Score: ", score))
    }
    else {
      quiz1 <<- 0
      # output$answers <- renderUI(paste("Score: ", quiz1))
    }
    removeModal()
  })
  observeEvent(input$submitq2, {
    if(input$q2 == "Nd5+") { 
      quiz2 <<- 1
      output$answers <- renderUI(paste("Score2: ", quiz2))
    }
    else {
      quiz2 <<- 0
      output$answers <- renderUI(paste("Score2: ", quiz2))
    }
    removeModal()
  })
  observeEvent(input$submitq3, {
    if(input$q3 == "cxb8=N+") {
      quiz3 <<- 1
      output$answers <- renderUI(paste("Score3: ", quiz3))
    }
    else {
      quiz3 <<- 0
      output$answers <- renderUI(paste("Score3: ", quiz3))
    }
    removeModal()
  })
  
 }

#Run the app
shinyApp(ui = ui, server = server)
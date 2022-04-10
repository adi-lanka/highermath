#EigenMat
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library("pracma")
library("expm")
library("rlang")
source("jaxmat.R")   #for displaying mathematics
# source("eigencalc.R")
# source("calceigen.R")

rake3x3 <- function() {
  repeat{
    c1 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    c2 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    c3 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    A <- cbind(c1,c2)
    A <- cbind(A, c3)
    if (det(A)== 0)
      next
    # if (det(A) <0)
    # {
    #     A <- cbind(c2,c1)
    #     A <- cbind(A, c3)
    # }
    trace <- A[1,1] + A[2,2] + A[3,3]
    print(trace)
    return(A)
  }
}

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Eigenvalues",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("Generate a 3x3 Matrix"),
      actionBttn("btn"),
      uiOutput("mat"),
      actionBttn("btn1")
    ),
    column(width=4,
           
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
  #
  eig <- character(0)
  matr <- rake3x3()
  
  A <- matrix(nrow = 3, ncol = 3)
  output$mat <- renderUI(jax.matrix(matr, name = "AHH"))
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)


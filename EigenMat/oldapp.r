#EigenMat
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library("pracma")
library("expm")
library("rlang")
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Find and check Eigenvalues of a random 3x3 Matrix",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=6,
      actionButton("genMat", "Generate Matrix", style = "color:blue"),
      uiOutput("showMat")
      # uiOutput("mata"),
      # uiOutput("char"),
      # uiOutput("cayley")
    ),
    column(width=6,
           actionButton("getEig", "Get Eigenvalue(s)", style = "color:green"),
           uiOutput("charEq"),
           uiOutput("var")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables
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
    # print(trace)
    return(A)
  }
}
charac <- function(lam, tr, det, a) {
  a <- a%^%2
  tr2 <- a[1,1] + a[2,2] + a[3,3]
  #ans <- (lam^3 - tr*lam^2 + 1/2*(tr - tr2*lam - det))
  # == 0
  if(round(lam^3 - tr*lam^2 + 1/2*((tr*tr - tr2)*lam) - det) %%5 == 0) {
    return(lam)
  }
  else {
    return(-1)
  }
}

cayley <- function(tr, det, a) {
  a2 <- a%^%2
  tr2 <- a2[1,1] + a2[2,2] + a2[3,3]
  #ans <- (lam^3 - tr*lam^2 + 1/2*(tr^2 - tr2*lam - det))
  # == 0
  return(round(a%^%3 - tr*(a%^%2) + (1/2)*((tr*tr - tr2)*a) - det*diag(3)))
}
server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  eig <- character(0) 
  A <- matrix(nrow = 3, ncol = 3)
  detm <- 0
  trace <- 0
  #Initialization
  observeEvent(input$genMat, {
    A <<- rake3x3()
    detm <<- det(A)
    trace <<- A[1,1] + A[2,2] + A[3,3]
    
    output$showMat <- renderUI(jax.matrix(A, "A"))
    for(i in 0:4) {
      #print(i)
      # eg2 <- charac(i, trace, detm, mat)
      # print(eg2)
      if(charac(i, trace, detm, A) >=0) {
        eig <<- c(eig, i)
      }
      # eig.append("four")
    }
    if(is_empty(eig)) {
      output$mata <- renderUI("No Eigenvalues found")
    }
    else {
      # output$charac <- renderUI(jaxI())
      #ans <- (lam^3 - tr*lam^2 + 1/2*(tr^2 - tr2*lam - det))
      output$charEq <- renderUI(jaxI("`lambda^3 -tr(A)*`lambda^2 + 1/2*((tr^2(A) - tr(A^2))`lambda - det(A)"))
      output$var <- renderUI(paste("Trace: ", trace))
      output$cayley <- renderUI(jax.matrix(cayley(trace, detm, A), "0 matrix"))
      
      output$mata <- renderUI(eig)
    }
    
  })
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)


#Fields
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
source("fieldsCalc.R")


stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Operations on Conformal Matrices",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
           radioButtons(inputId = "operation", label = "Operation",
                        choices = c("+","-","/", "*")),
           uiOutput("mat1"),
           uiOutput("mat2"),
           uiOutput("mat3"),
           uiOutput("mat4"),
           uiOutput("mat5"),
           uiOutput("mat6"),
           uiOutput("mat7"),
           uiOutput("mat8"),
           uiOutput("mat9"),
           
    ),
    column(width = 4,
           selectInput("mat1", "Choose left matrix:",
                       list("Matrix 1" = 1,
                            "Matrix 2" = 2,
                            "Matrix 3" = 3,
                            "Matrix 4" = 4,
                            "Matrix 5" = 5,
                            "Matrix 6" = 6,
                            "Matrix 7" = 7,
                            "Matrix 8" = 8,
                            "Matrix 9" = 9
                       ))
           ),
    column(width = 4,
           selectInput("mat2", "Choose right matrix:",
                       list("Matrix 1" = 1,
                            "Matrix 2" = 2,
                            "Matrix 3" = 3,
                            "Matrix 4" = 4,
                            "Matrix 5" = 5,
                            "Matrix 6" = 6,
                            "Matrix 7" = 7,
                            "Matrix 8" = 8,
                            "Matrix 9" = 9
                       )),
           actionButton("submit", "Submit"),
           uiOutput("answer")
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
  
  #Initialization
  
  #Functions that respond to events in the input
  output$mat1 <- renderUI(jax.matrix(mat[,,1], paste0("m_", 1)))
  output$mat2 <- renderUI(jax.matrix(mat[,,2], paste0("m_", 2)))
  output$mat3 <- renderUI(jax.matrix(mat[,,3], paste0("m_", 3)))
  output$mat4 <- renderUI(jax.matrix(mat[,,4], paste0("m_", 4)))
  output$mat5 <- renderUI(jax.matrix(mat[,,5], paste0("m_", 5)))
  output$mat6 <- renderUI(jax.matrix(mat[,,6], paste0("m_", 6)))
  output$mat7 <- renderUI(jax.matrix(mat[,,7], paste0("m_", 7)))
  output$mat8 <- renderUI(jax.matrix(mat[,,8], paste0("m_", 8)))
  output$mat9 <- renderUI(jax.matrix(mat[,,9], paste0("m_", 9)))
  
  observeEvent(input$submit,{
    ans <- calc(input$operation, as.numeric(input$mat1), as.numeric(input$mat2))
    output$answer <- renderUI(jax.matrix(ans, "Result:"))
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)
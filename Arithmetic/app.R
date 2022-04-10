#Arithmetic
#August 30, 2020
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    h1 {
      color: purple;
      background-color: white;
      font-style: italic
    }
    h2 {
      color: red;
      background-color: white;
      font-style: cursive
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "A trivial reactive app")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
        column(width = 6,
               radioButtons(inputId = "operation", label = "Operation",
                            choices = c("Sum", "Product")),
            uiOutput("oper"),
            sliderInput("left", "Left operand",-5, 5, 1 ),
            sliderInput("right", "Right operand",-5, 5, 1 ),
            h2(uiOutput("results"))
        ),
        column(width = 4,
               h1(uiOutput("operation")),
               h1(uiOutput("equation")))
    )
)
ui <- dashboardPage(header, sidebar, body)



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
#This works without any explicit reactive function
   # output$results <- renderUI(paste0("The ",input$left+input$right, "is "))
#The next two lines fail
# sum <- input$left+input$right     #not allowed
# output$results <- renderUI(paste0("Sum is ",sum)) 
   
#  Use a reactive expression instead and it works  
   # //////////
  product <- reactive({input$left*input$right})
  sum <- reactive({input$left+input$right})
  #   observeEvent(input$operation), {
  #       if(input$operation)
  #   }
  # output$results <- renderUI(paste0("Sum is ",sum()))
  
  latex_output <- reactive(
      if(input$operation == "Sum") {
          "x + y = "
      }
      else {
          "xy = "
      }
  )
      
  observeEvent(input$operation, 
    {
        if(input$operation == "Sum") {
            output$operation <- renderUI(paste("The ", input$operation, " is", sum())) 
            output$equation <- renderUI(
                jaxI(paste0("x = ", input$left, ",y = ", input$right, " ,", latex_output(), " ", sum())))                                 
        }
        
        else {
            output$operation <- renderUI(paste("The ", input$operation, " is", product()))
            output$equation <- renderUI(
                jaxI(paste0("x = ", input$left, ",y = ", input$right, " ,", latex_output(), " ", product())))
        }
      
  })
  
# Defining an ordinary function also works
 # sum <- function() input$left+input$right #Shiny must know this is reactive and reevaluate it
 # output$results <- renderUI(paste0("Sum is ",sum()))
   #Here is a way to create a list of values that respond to changing inputs
 #  myReactives <- reactiveValues()
   #You need observe() to create a reactive context
 #  observe({myReactives$sum <- input$left+input$right
 #  myReactives$left <- input$left
 #  myReactives$right <- input$right
 #  })
 #  output$results <- renderUI(paste0("Sum of ",myReactives$left,
 #                                    " and ", myReactives$right, " is ",myReactives$sum))


}

#Run the app
shinyApp(ui = ui, server = server)
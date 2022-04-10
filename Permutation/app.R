#Permutation
#Change the name to Permutation when you finish the homework
library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("jaxmat.R")
source("permutecalc.R")

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    h3 {
      color: red;
      background-color: white;
      font-style: bold
    }
    h4 {
      color: purple;
      font-style: italic
    }
  ')
))
# tags$head(tags$style("#powersa{color:red; font-size:20px;
#             font-style:italic;
#             overflow-y:scroll; max-height:250px;}"))

header <- dashboardHeader(title = "Permutations")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(
      width = 4,
      box(
          width = NULL, height = 220,
          h3("Input"),
          textInput("atext","a","(12)"),
          textInput("btext","b","(13)")
      ),
      box(
          width = NULL, height = 150,
          h3("Products"),
          h4(uiOutput("prodab")),
          h4(uiOutput("prodba"))
      ),
      box(
        width = NULL, height = 200,
        h3("Inverses"),
        h4(jaxI("a^{-1}:"), uiOutput("inverseA")),
        h4(jaxI("b^{-1}:"),uiOutput("inverseB")),
      ),
      box(
        width = NULL, height = 200,
        h3("Conjugates"),
        h4(jaxI("aba^{-1}:"),uiOutput("conjugateAB")),
        h4(jaxI("bab^{-1}:"),uiOutput("conjugateBA")),
        tags$head(tags$style("#conjugateAB{
            overflow-y:scroll; max-height:250px;}"))
      )
    ),
    column(
        width = 4,
        box(
            width = NULL, height = 200,
            h3("Powers of a"),
            h4(uiOutput("powersa")),
            tags$head(tags$style("#powersa{
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 200,
          h3("Powers of AB"),
          h4(uiOutput("powersAB")),
          tags$head(tags$style("#powersAB{
            overflow-y:scroll; max-height:250px;}"))
        )
    ),
    column(
        width = 4,
        box(
          width = NULL, height = 200,
          h3("Powers of b"),
          h4(uiOutput("powersb")),
          tags$head(tags$style("#powersb{
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 200,
          h3("Powers of BA"),
          h4(uiOutput("powersBA")),
          tags$head(tags$style("#powersBA{
            overflow-y:scroll; max-height:250px;}"))
        ),
        actionBttn("btncalc","Calculate",
            color = "primary", size = "lg"), #an awesome button from shinyWidgets
        
    )
  )    
)

ui <- dashboardPage(header, sidebar, body)


source("permutecalc.R")    


server <- function(input, output) {
  output$inverseA <- renderUI("(12)")
  output$inverseB <- renderUI("(13)")
  output$prodab <- renderUI("ab = (132)")
  output$prodba <- renderUI("ba = (123)")
  output$powersa <- renderUI(HTML(paste("(12)","I",sep = "<br/>")))
  output$powersb <- renderUI(HTML(paste("(13)","I",sep = "<br/>")))
  output$conjugateAB <- renderUI("(23)")
  output$conjugateBA  <- renderUI("(23)")
  output$powersAB <- renderUI(HTML(paste("(132)","(123)","I",sep = "<br/>")))
  output$powersBA <- renderUI(HTML(paste("(123)","(132)","I",sep = "<br/>")))
  observeEvent(input$btncalc, {
    ab <- Perm.multiply(input$atext,input$btext)
    output$prodab <- renderUI(paste("ab =  ",ab))
    ba <- Perm.multiply(input$btext,input$atext)
    output$prodba <- renderUI(paste("ba =  ",ba))
    output$powersa <- renderUI(HTML(Perm.powerString(input$atext)))
    output$powersb <- renderUI(HTML(Perm.powerString(input$btext)))
    output$powersAB <- renderUI(HTML(Perm.powerString(ab)))
    output$powersBA <- renderUI(HTML(Perm.powerString(ba)))
    output$inverseA <- renderUI(Perm.inverse(input$atext))
    output$inverseB <-renderUI(Perm.inverse(input$btext))
    output$conjugateAB <- renderUI(Perm.conjugate(input$atext, input$btext))
    output$conjugateBA <- renderUI(Perm.conjugate(input$btext, input$atext))
  
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

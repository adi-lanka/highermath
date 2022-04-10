#IrreduciblePoly
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
source("Calcpoly.R") #back end calculations
source("fieldcalc.R") #Prof's code
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Irreducible Polynomials and Generators",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("This app tests the 10 irreducible polynomials for p=5,n=2 and evaluates if any of them are generators.")
    )
  ),
  fluidRow(stylesheet,
           column(width=4,
                  actionButton("poly1", "x^2 + 2"),
                  actionButton("poly2", "x^2 + 3"),
                  actionButton("poly3", "x^2 + x + 1"),
                  actionButton("poly4", "x^2 + x + 2"),
                  actionButton("poly5", "x^2 + 2x + 3"),
                  actionButton("poly6", "x^2 + 2x + 4"),
                  actionButton("poly7", "x^2 + 3x + 3"),
                  actionButton("poly8", "x^2 + 3x + 4"),
                  actionButton("poly9", "x^2 + 4x + 1"),
                  actionButton("poly10", "x^2 + 4x + 2")
                  
           ),
           column(width=2,
                  uiOutput("varpower")
                  ),
           column(width=4,
                  uiOutput("replace"),
                  uiOutput("isgen"),
                  uiOutput("powers")
                  
            )))
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
  #Output function to use in event handlers
  gen.output <- function(poly){
    powers <- gen.powers(poly)
    output$powers <- renderUI("")
    output <- character(0)
    for(i in c(1:24)){
      #Switch order of coeffs sinice convertploy goes low to high, and we go high to low
      # pow <- paste0(c(powers[i, 1], "x + ", powers[i, 2]))
      # output <- paste(output,pow, sep="\n")
      # output <- paste(output,convertPoly(c(powers[i, 2], powers[i, 1])), sep="<br>")      
      line <- paste0("x^", i-1, " = ", convertPoly(c(powers[i, 2], powers[i, 1])))
      output <- paste(output, line, sep="<br>")
    }
    print(output)
    return(output)
  }
  
  #Event handler for poly 1:
  observeEvent(input$poly1, {
    replace <- gen.replacement(c(1,0,2))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    # output$replace <- gen.replacement(c(1,0,2))
    output2 <- gen.output(c(1, 0, 2))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  }) 
  observeEvent(input$poly2, {
    replace <- gen.replacement(c(1,0,3))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 0, 3))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly3, {
    replace <- gen.replacement(c(1,1,1))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 1, 1))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly4, {
    replace <- gen.replacement(c(1,1,2))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 1, 2))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly5, {
    replace <- gen.replacement(c(1,2,3))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 2, 3))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly6, {
    replace <- gen.replacement(c(1,2,4))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 2, 4))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly7, {
    replace <- gen.replacement(c(1,3,3))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 3, 3))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly8, {
    replace <- gen.replacement(c(1,3,4))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 3, 4))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly9, {
    replace <- gen.replacement(c(1,4,1))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 4, 1))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  observeEvent(input$poly10, {
    replace <- gen.replacement(c(1,4,2))
    output$replace <- renderUI(paste0("x^2 = ", convertPoly(c(replace[2], replace[1]))))
    output2 <- gen.output(c(1, 4, 2))
    output$isgen <- renderUI(ifelse(is.gen, "This is a generator", "This is not a generator"))
    output$powers <- renderUI(HTML(output2)) 
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)
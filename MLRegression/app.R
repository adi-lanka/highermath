#MLRegression
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
#Load the dataset
# CER <- read.csv("cereal.csv")
# CER
#Pitchers
pitch <- get("Pitchers2005")
pitch
# data(package="resampledata")
# quake <- get("Bushmeat")
# quake
# dset <- subset(CER,TRUE,select=c(name,calories,sodium,fiber,protein, fat,sugars,rating))
dset <- subset(pitch,TRUE,select=c(Name,Wins,Losses,Saved,Innings, Walks, Strikeouts,ERA))
# dset
# nrow(dset)



#The user interface
header <- dashboardHeader(title = "Linear Regression ",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE
  
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 6,
           tableOutput("tbl")  #display the data set
    ),
    column(width = 3,
       h3("Single Linear Regression"),
       selectInput("select","Predictor",choices = c("Wins", "Losses","Saved", "Innings", "Walks", "Strikeouts")),
       plotOutput("scatter"),
       h3(uiOutput("eqn")),
       h3(uiOutput("resid")),
       h3(uiOutput("corr"))
       
          
    ),
    column(width = 3,
       h3("Multiple Linear Regression"),
       checkboxInput("Wins","Wins"),
       checkboxInput("Losses","Losses"),
       checkboxInput("Saved","Saved"),
       checkboxInput("Innings","Innings"),
       checkboxInput("Walks","Walks"),
       checkboxInput("Strikeouts","Strikeouts"),
       h3(uiOutput("resid2")),
       h3(uiOutput("coeff1")),
       h3(uiOutput("coeff2")),
       h3(uiOutput("coeff3")),
       h3(uiOutput("coeff4")),
       h3(uiOutput("coeff5")),
       h3(uiOutput("coeff6")),
       h3(uiOutput("coeff7"))
       
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  predictors <- c( FALSE,FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  output$tbl <- renderTable(dset)
  observeEvent(input$select,{
    predictor <- input$select
    #Find the regression line by projection
    A <- cbind(rep(1,nrow(dset)),dset[,predictor])
    B <- t(A)%*%A
    P <- A%*%solve(B)%*%t(A)
    y.hat <- P%*%dset$ERA     #predicted values, on the regression line
    output$scatter <- renderPlot({plot(dset[,predictor],dset$ERA,pch = ".",cex = 3)  #scatter plot of the data
    points(dset[,predictor],y.hat,type = "b")
    })
    
    coeff <- solve(B)%*%t(A)%*%dset$ERA;coeff
    output$eqn <- renderUI(HTML(paste("The equation of the regression line is",br(), 
                                 "y = ",round(coeff[2],3),"x + ", round(coeff[1],3))))
                                 
    #Here is the length of the shortest vector to the 2D subspace
    lenResid <- sqrt(sum((dset$ERA-y.hat)^2))
    output$resid <- renderUI(HTML(paste("The length of the vector of residuals is",br(),round(lenResid,3))))
    corr <- cor(dset$ERA,dset[,predictor])   
    output$corr <- renderUI(paste("The correlation between predictor and ERA is",round(corr,3)))
    
  })
  
  doMultiple <- function(){
    vpred <- which(predictors == TRUE)
    if (length(vpred) == 0) return()
    A <- as.matrix(cbind(rep(1,nrow(dset)),dset[,vpred]))
    B <- t(A)%*%A
    P <- A%*%solve(B)%*%t(A)
    y.hat <- P%*%dset$ERA     #predicted values, on the regression line
    lenResid <- sqrt(sum((dset$ERA-y.hat)^2))
    output$resid2 <- renderUI(HTML(paste("The length of the vector of residuals is",br(),round(lenResid,3))))
    coeff <- solve(B)%*%t(A)%*%dset$ERA
    output$coeff1 <- renderUI(paste("Constant =", round(coeff[1],3)))
    if (2 %in% vpred)
      output$coeff2 <- renderUI(paste("Coefficient for calories =", round(coeff[1+which.max(vpred==2)],3)))
    else output$coeff2 <- NULL 
    if (3 %in% vpred)
      output$coeff3 <- renderUI(paste("Coefficient for sodium =", round(coeff[1+which.max(vpred==3)],3)))
    else output$coeff3 <- NULL
    if (4 %in% vpred)
      output$coeff4 <- renderUI(paste("Coefficient for fiber =", round(coeff[1+which.max(vpred==4)],3)))
    else output$coeff4 <- NULL
    if (5 %in% vpred)
      output$coeff5 <- renderUI(paste("Coefficient for protein =", round(coeff[1+which.max(vpred==5)],3)))
    else output$coeff5 <- NULL
    if (6 %in% vpred)
      output$coeff6 <- renderUI(paste("Coefficient for fat =", round(coeff[1+which.max(vpred==6)],3)))
    else output$coeff6 <- NULL
    if (7 %in% vpred)
      output$coeff7 <- renderUI(paste("Coefficient for sugar =", round(coeff[1+which.max(vpred==7)],3)))
    else output$coeff7 <- NULL
  }
  observeEvent(input$Wins,{
    predictors[2] <<- input$Wins
    doMultiple()
  })
  # observeEvent(input$Losses,{
  #   predictors[3] <<- input$Losses
  #   doMultiple()
  # })
  observeEvent(input$Losses,{
    predictors[3] <<- input$Losses
    doMultiple()
  })
  observeEvent(input$Saved,{
    predictors[4] <<- input$Saved
    doMultiple()
  })
  # ("Wins","Losses", "Saved", "Innings", "Walks", "Strikeouts", "ERA")
  observeEvent(input$Innings,{
    predictors[5] <<- input$Innings
    doMultiple()
  })
  observeEvent(input$Walks,{
    predictors[6] <<- input$Walks
    doMultiple()
  })
  observeEvent(input$Strikeouts,{
    predictors[7] <<- input$Strikeouts
    doMultiple()
  })
  

}

#Run the app
shinyApp(ui = ui, server = server)
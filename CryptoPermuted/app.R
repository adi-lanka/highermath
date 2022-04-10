#Probably CryptoPermute
#CryptoFourier
#TODO: change formatting and remove references to alcohol
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
# help("resampledata")
# data(package="resampledata")
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#Load the dataset
# dset <- get("Cereals")
# dset <- get("Alelager")
# dset
dset <- read.csv("Ethereum.csv")
dset
# dset2 <- get("Beerwings")
# dset2
# dset
#Clean up the dataset if necessary


#The user interface
header <- dashboardHeader(title = "Permutation testing",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 200,
  radioButtons("beerhot", "Metric",choices = c("logprice","Vol.")),
  actionBttn("btnnew","New permuted dataset")
  
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 3,
           h3("Analysis of actual dataset"),
           plotOutput("trueplot"),
           uiOutput("truediff"),
           uiOutput("trueratio"),
           uiOutput("truemed"),
           h3("Analysis of permuted dataset"),
           plotOutput("scrambleplot"),
           uiOutput("scramdiff"),
           uiOutput("scramratio"),
           uiOutput("scrammed")
    ),
    column(width = 3,
      h3("Permutation test"),
      sliderInput("nsample","Number of permuted samples",1000,20000,5000),
      radioButtons("btnstat","Statistic",
                   choiceNames = c("Difference of means","Ratio of means","Difference of medians"),
                   choiceValues = c("diff","ratio","mdiff")),
      actionBttn("btntest","Conduct the test"),
      plotOutput("hist"),
      uiOutput("pval")
    )
  ),
  fluidRow(stylesheet, 
           column(width = 5,
                  h3("The actual dataset"),
                  tableOutput("tbl")  #display the data set
           ),
           column(width = 5,
                  h3("A permuted dataset"),
                  tableOutput("scramtbl")
                  
           ))
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


#Additional functions are OK here, but no variables
doPlot <- function(dset, consume) {
  formula <- as.formula(paste(consume,"~","value"))
  boxplot(formula, dset)
}
scramble <- function(dset){
  dset$value <- sample(dset$value)
  return(dset)
}

doPermtest <- function(dset,consume,nPerm,stat){
  result <- numeric(nPerm)
  for (i in 1:nPerm){
    permset <- scramble(dset)
    men <- which(permset$value == "Weekend")
    result[i] <- switch(stat,
      diff =mean(permset[men,consume])-mean(permset[-men,consume]),
      ratio = mean(permset[,consume][men])/mean(permset[,consume][-men]),
      mdiff = median(permset[,consume][men])-median(permset[,consume][-men])
    )
  }
  print(result)
  return (result)
}

#result <- doPermtest(dset,"Beer",10,"mdiff");result


server <- function(session, input, output) {
  
  make.eth.weekend <- function(){
    dates <- as.Date(dset$Date)
    weekday <- format(dates, '%A') # Get day of the week
    is.weekend <- function(day){
      if(day == "Friday" || day == "Saturday" || day == "Sunday"){ # Is capitilazation right? Check!
        return("Weekend")
      } else {
        return("Weekday") # Maybe add error checking here?
      }
    }
    #weekends = [is.weekend(d) for d in weekdays]

    is.weekend <- Vectorize(is.weekend) # Need to be able to run on a vector input
    weekends <- is.weekend(weekday)
    print(weekends)
    # weekends <- c("Weekend", weekends) # Add label for the DF
    
    # DF <- data.frame(weekends)
    DF <- data.frame(value=weekends, row.names=NULL)
    dset <<- cbind(dset, DF)
    print(dset)
  }
  make.eth.weekend()
  
 convert.log.price <- function() {
   dset <<- cbind(dset, logprice=log(dset$Price))
 }
 convert.log.price()
 print(dset)
  consume <- "logprice"
  #change columns
  output$tbl <- renderTable(dset[c("Open", "Vol.")])
  permset <- scramble(dset)
  #change permuted columns
  output$scramtbl <- renderTable(permset)
  analyze <- function(dset,consume,realdata){
    if (realdata){
      output$trueplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$value == "Weekend")
      output$truediff <- renderUI(h4(paste("Difference in means =",mean(dset[men,consume])-mean(dset[-men,consume]))))
      output$trueratio <- renderUI(h4(paste("Ratio of means =",
                                    round(mean(dset[men,consume])/mean(dset[-men,consume]),digits =2))))
      output$truemed <- renderUI(h4(paste("Difference in medians =",
                                    median(dset[men,consume])-median(dset[-men,consume]))))
    }
    else {
      output$scrambleplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$value == "Weekday")
      print(men)
      output$scramdiff <- renderUI(h4(paste("Difference in means =",
                                            mean(dset[men,consume])-mean(dset[-men,consume]))))
      output$scramratio <- renderUI(h4(paste("Ratio of means =",
                                          round(mean(dset[men,consume])/mean(dset[-men,consume]),digits =2))))
      output$scrammed <- renderUI(h4(paste("Difference in medians =",
                                        median(dset[men,consume])-median(dset[-men,consume]))))
    }
  }
  analyze(dset,consume,TRUE)
  analyze(permset,consume,FALSE)
  
  observeEvent(input$beerhot,{
    consume <<- input$beerhot
    analyze(dset,consume,TRUE)
    analyze(permset,consume,FALSE)
  })
  observeEvent(input$btnnew,{
    permset <<- scramble(dset)
    output$scramtbl <- renderTable(permset)
    analyze(permset,consume,FALSE)
  })
  
  observeEvent(input$btntest,{
    result <- doPermtest(dset,consume,input$nsample,input$btnstat)
    men <- which(dset$value == "Weekend")
    vline <- switch(input$btnstat,
                    diff = mean(dset[men,consume])-mean(dset[-men,consume]),
                    ratio = mean(dset[men,consume])/mean(dset[-men,consume]),
                    mdiff = median(dset[men,consume])-median(dset[-men,consume])
    )
    output$hist <- renderPlot({
      hist(result)
      abline(v = vline, col = "blue")
    })
    pvalue <- (sum(result >= vline )+1)/(input$nsample+1)
    output$pval <- renderUI(h3(paste("Pvalue =", pvalue)))
  })
  
  

}

#Run the app
shinyApp(ui = ui, server = server)
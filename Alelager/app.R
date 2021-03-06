#AleLager
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
  radioButtons("beerhot", "Consumable",choices = c("Alcohol","Calories")),
  actionBttn("btnnew","New permuted dataset")
  
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 3,
           h3("The actual dataset"),
           tableOutput("tbl")  #display the data set
    ),
    column(width = 3,
           h3("A permuted dataset"),
           tableOutput("scramtbl")
          
    ),
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
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


#Additional functions are OK here, but no variables
doPlot <- function(dset, consume) {
  formula <- as.formula(paste(consume,"~","Type"))
  boxplot(formula, dset)
}
scramble <- function(dset){
  dset$Type <- sample(dset$Type)
  return(dset)
}

doPermtest <- function(dset,consume,nPerm,stat){
  result <- numeric(nPerm)
  for (i in 1:nPerm){
    permset <- scramble(dset)
    men <- which(permset$Type == "Ale")
    result[i] <- switch(stat,
      diff =mean(permset[men,consume])-mean(permset[-men,consume]),
      ratio = mean(permset[,consume][men])/mean(permset[,consume][-men]),
      mdiff = median(permset[,consume][men])-median(permset[,consume][-men])
    )
  }
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
    
    is.weekend <- Vectorize(is.weekend) # Need to be able to run on a vector input
    weekends <- is.weekend(weekday)
    weekends <- c("Weekend", weekends) # Add label for the DF
    
    DF <- data.frame(weekends)
    dset <<- rbind(dset, DF)
  }
  make.eth.weekend()
  print(dset$Weekend)
  --------
  consume <- "Alcohol"
  output$tbl <- renderTable(dset)
  permset <- scramble(dset)
  output$scramtbl <- renderTable(permset)
  analyze <- function(dset,consume,realdata){
    if (realdata){
      output$trueplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$Type == "Ale")
      output$truediff <- renderUI(h4(paste("Difference in means =",mean(dset[men,consume])-mean(dset[-men,consume]))))
      output$trueratio <- renderUI(h4(paste("Ratio of means =",
                                    round(mean(dset[men,consume])/mean(dset[-men,consume]),digits =2))))
      output$truemed <- renderUI(h4(paste("Difference in medians =",
                                    median(dset[men,consume])-median(dset[-men,consume]))))
    }
    else {
      output$scrambleplot <- renderPlot({doPlot(dset,consume)})
      men <- which(dset$Type == "Ale")
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
    men <- which(dset$Type == "Ale")
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
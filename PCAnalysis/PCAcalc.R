#CryptoPCAnalysis
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)
# help("resampledata")
# data(package="resampledata")

#source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Principal Component Analysis on Etherem and Bitcoin",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
             column(width=2,
                    selectInput("regions", "Years", NULL)
             ),
             column(width = 5,
                    h3("Ethereum Prices over time"),
                    plotOutput("cases"),
                    sliderInput("nRecon", "Eigenvectors for Reconstruction",min = 1, max = 45, value = 4),
                    uiOutput("resid"),
                    uiOutput("coef1"),
                    uiOutput("coef2"),
                    uiOutput("coef3"),
                    uiOutput("coef4"),
                    uiOutput("coef5"),
                    uiOutput("coef6"),
                    uiOutput("coef7"),
                    uiOutput("coef8"),
                    uiOutput("coef9"),
                    uiOutput("coef10"),
             ),
             column(width = 5,
                    
                    h3("Top eigenvectors"),
                    sliderInput("numshow", "Number to show", min = 2, max = 10,value = 3),
                    plotOutput("eigen"),
                    uiOutput("val1"),
                    uiOutput("val2"),
                    uiOutput("val3"),
                    uiOutput("val4"),
                    uiOutput("val5"),
                    uiOutput("val6"),
                    uiOutput("val7"),
                    uiOutput("val8"),
                    uiOutput("val9"),
                    uiOutput("val10")
                    
             )
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


####Data####
##Globals##
nSets <- 5
daysPerSet <- 365
# TODO: "eth" -> "dset"
dset <- read.csv("Ethereum.csv")
# Can assign dset as global later


## COVID <- read.csv("COVIDcases.csv")
## nSets
## COVID[1:daysPerSet,4]
## lottery <- get("Pitchers2005")
## lottery
## print(eth[1:daysPerSet,3])


#Functions to get access to the numeric data for a region
extract <- function(x) {
    if(x>nSets){
        #TODO: Error handling
    }
    #TODO: figure out what 3 means, change to variable
    #TODO: Sanity check formula
    return(rev(dset[(1+daysPerSet*(x-1)):(daysPerSet+daysPerSet*(x-1)),3]))
}
## extract <- function(x) rev(eth[365,3],3)
avg <- function(x){
    if(x>nSets){
        #TODO: Error handling
    }
    return(mean(extract(x)))            #their average
}
## name <- function(x) paste(eth[1+84*(x-1),1]," ",eth[1+84*(x-1),2])
name <- function(x) {
    if(x>nSets){
        #TODO: Error handling
    }
    #TODO: figure out what 2 means, change to variable
    return(dset[1+daysPerSet*(x-1),2])
}

## extract(33)
## name(33)


####Calculation####
A <- matrix(daysPerSet*nSets,nrow = daysPerSet, ncol = nSets)      #make an empty matrix
#Center and rescale the data, one column per region
for (i in 1:nSets){
    A[,i] <- (extract(i)-avg(i))/sqrt(daysPerSet)
}
#Make a symmetric square matrix
S <- A%*%t(A)
Eig <- eigen(S)
Eig$values  #first few Coefficients are much larger than the others
P <- -Eig$vectors  #change the sign to get more positive coefficients



#Now we can express each column of A relative to the new basis of eigenvectors. 
#The complete set of coefficients will provide perfect reconstruction
PInv <- solve(P)
A.eig <- PInv%*%A
A.eig[,2]

#We can reconstruct the original data for one region using all the eigenvectors
k <- 2
name(k)
recon <- as.numeric(P%*%A.eig*sqrt(daysPerSet)+avg(k))
#plot(1:daysPerSet,recon,xlab = name(k), type = "l")
#points(1:daysPerSet,extract(k))


####Server####
server <- function(session, input, output) {
    colors <- c("black", "red", "green", "blue", "magenta", "gray", "orange", "brown", "purple", "cyan")
    index <- 1
    P <- -Eig$vectors  #change the sign to get more positive coefficients
    PInv <- solve(P)
    A.eig <- PInv%*%A
    regions <- character(nSets)
    for (i in 1:nSets)
        regions[i] <- trimws(name(i))
    updateSelectInput(session,"regions",choices = regions, selected = "Italy")
    D <- round(Eig$values)
    output$eigen <- renderPlot({
        plot(1:daysPerSet, P[,1], ylim = c(-0.4,0.4),type = "l")
        for (i in 2:input$numshow)
            points(1:daysPerSet, P[,i], type = "l",col = colors[i])
        
    })
    
    output$val1 <- renderUI(h4(paste("Eigenvalue 1: ",D[1]), style = paste0("color:",colors[1]) ))
    output$val2 <- renderUI(h4(paste("Eigenvalue 2: ",D[2]), style = paste0("color:",colors[2]) ))
    output$val3 <- renderUI(h4(paste("Eigenvalue 3: ",D[3]), style = paste0("color:",colors[3]) ))
    output$val4 <- renderUI(h4(paste("Eigenvalue 4: ",D[4]), style = paste0("color:",colors[4]) ))
    output$val5 <- renderUI(h4(paste("Eigenvalue 5: ",D[5]), style = paste0("color:",colors[5]) ))
    output$val6 <- renderUI(h4(paste("Eigenvalue 6: ",D[6]), style = paste0("color:",colors[6]) ))
    output$val7 <- renderUI(h4(paste("Eigenvalue 7: ",D[7]), style = paste0("color:",colors[7]) ))
    output$val8 <- renderUI(h4(paste("Eigenvalue 8: ",D[8]), style = paste0("color:",colors[8]) ))
    output$val9 <- renderUI(h4(paste("Eigenvalue 9: ",D[9]), style = paste0("color:",colors[9]) ))
    output$val10 <- renderUI(h4(paste("Eigenvalue 10: ",D[10]), style = paste0("color:",colors[10]) ))
    
    showCases <- function(casedata, rName){
        plot(1:daysPerSet,casedata,xlab = rName, type = "l")
    }
    observeEvent(input$regions, {
        index <<- which.max(regions == input$regions)
        output$cases <- renderPlot(showCases(extract(index),input$regions))
        output$coef1 <- renderUI(h4(paste("Coefficient 1: ",A.eig[1,index]), style = paste0("color:",colors[1]) ))
        output$coef2 <- renderUI(h4(paste("Coefficient 2: ",A.eig[2,index]), style = paste0("color:",colors[2]) ))
        output$coef3 <- renderUI(h4(paste("Coefficient 3: ",A.eig[3,index]), style = paste0("color:",colors[3]) ))
        output$coef4 <- renderUI(h4(paste("Coefficient 4: ",A.eig[4,index]), style = paste0("color:",colors[4]) ))
        output$coef5 <- renderUI(h4(paste("Coefficient 5: ",A.eig[5,index]), style = paste0("color:",colors[5]) ))
        output$coef6 <- renderUI(h4(paste("Coefficient 6: ",A.eig[6,index]), style = paste0("color:",colors[6]) ))
        output$coef7 <- renderUI(h4(paste("Coefficient 7: ",A.eig[7,index]), style = paste0("color:",colors[7]) ))
        output$coef8 <- renderUI(h4(paste("Coefficient 8: ",A.eig[8,index]), style = paste0("color:",colors[8]) ))
        output$coef9 <- renderUI(h4(paste("Coefficient 9: ",A.eig[9,index]), style = paste0("color:",colors[9]) ))
        output$coef10 <- renderUI(h4(paste("Coefficient 10: ",A.eig[10,index]), style = paste0("color:",colors[10]) ))
        
    })
    observeEvent(input$nRecon,{
        n <- input$nRecon
        if (n == 1)
            recon <- as.numeric(P[,1]*A.eig[1,index]*sqrt(daysPerSet)+avg(index))
        else {
            B <- A.eig[1:n,index]
            recon <- as.numeric(P[,1:n]%*%B*sqrt(daysPerSet)+avg(index))
        }
        output$cases <- renderPlot({
            showCases(extract(index),input$regions)
            points(1:daysPerSet,recon,xlab = name(index), type = "b", col = "red")
        })
        lenResid <- sqrt(sum((extract(index)-recon)^2))
        output$resid <- renderUI(h4(paste("Length of Residuals = ",round(lenResid))))
        output$coef1 <- renderUI(h4(paste("Coefficient 1: ",A.eig[1,index]), style = paste0("color:",colors[1]) ))
        output$coef2 <- renderUI(h4(paste("Coefficient 2: ",A.eig[2,index]), style = paste0("color:",colors[2]) ))
        output$coef3 <- renderUI(h4(paste("Coefficient 3: ",A.eig[3,index]), style = paste0("color:",colors[3]) ))
        output$coef4 <- renderUI(h4(paste("Coefficient 4: ",A.eig[4,index]), style = paste0("color:",colors[4]) ))
        output$coef5 <- renderUI(h4(paste("Coefficient 5: ",A.eig[5,index]), style = paste0("color:",colors[5]) ))
        output$coef6 <- renderUI(h4(paste("Coefficient 6: ",A.eig[6,index]), style = paste0("color:",colors[6]) ))
        output$coef7 <- renderUI(h4(paste("Coefficient 7: ",A.eig[7,index]), style = paste0("color:",colors[7]) ))
        output$coef8 <- renderUI(h4(paste("Coefficient 8: ",A.eig[8,index]), style = paste0("color:",colors[8]) ))
        output$coef9 <- renderUI(h4(paste("Coefficient 9: ",A.eig[9,index]), style = paste0("color:",colors[9]) ))
        output$coef10 <- renderUI(h4(paste("Coefficient 10: ",A.eig[10,index]), style = paste0("color:",colors[10]) ))
        
        
    })
}

#Run the app
shinyApp(ui = ui, server = server)



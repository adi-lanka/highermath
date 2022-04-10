#CryptoQuiz
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
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
header <- dashboardHeader(title = "Crypto Fill in the Blank Quiz",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),  #enable onclick 
  
  fluidRow(stylesheet,
    
    column(width=4,
           h3("When prompted please guess Bitcoin, Ethereum, or Solana"),
           h3("Click on GUESS for the answer"),
           uiOutput("over1"),
          textOutput("guess1"),
          uiOutput("over2"),
          textOutput("guess2"),
          uiOutput("over3"),
          textOutput("guess3"),
          uiOutput("over4"),
          textOutput("guess4"),
          uiOutput("over5"),
          textOutput("guess5"),
          uiOutput("over6"),
          textOutput("guess6"),
          uiOutput("over7"),
          textOutput("guess7"),
          uiOutput("over8"),
          textOutput("guess8"),
          uiOutput("over9"),
          textOutput("guess9"),
          uiOutput("over10"),
          textOutput("guess10"),
          uiOutput("over11"),
          textOutput("guess11"),
          uiOutput("over12"),
          textOutput("guess12"),
          uiOutput("over13"),
          textOutput("guess13"),
          uiOutput("over14"),
          textOutput("guess14"),
          uiOutput("over15"),
         ),
    column(width=4,
           img(src="btc2.jpg", width = "75%"),
           img(src="eth.jpg", width = "75%"),
           img(src="solana.jpg", width = "75%"))
    
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
  output$over1 <- renderText("Before ")
  output$guess1 <- renderText("GUESS")
  output$over2 <- renderText(", there was no way to achieve consensus (for example on the state of a ledger of transactions, or any database) without having a central authority determining the state.  The amazing innovation that came from")
  output$guess2 <- renderText("GUESS")
  output$over3 <- renderText("is called Nakamoto consensus which allows the network to achieve consensus in a robust and probabilistic way, without any central point of failure.  

Another interesting feature is that it creates a decentralized network where anybody can be the root user of a multi-client database which is a non-trivial accomplishment.  The results of the consensus are stored block by block on what is a distributed database known as a blockchain.  One benefit of using 
")
  output$guess3 <- renderText("GUESS")
  output$over4 <- renderText("as a currency is that it is hard money and nobody can just print as much as they would like whenever there is a need, like the United States government whenever it is in a recession or during the pandemic. ")
  output$guess4 <- renderText("GUESS")
 
  output$guess5 <- renderText("GUESS")
  output$guess6 <- renderText("GUESS")
  output$guess7 <- renderText("GUESS")
  output$guess8 <- renderText("GUESS")
  output$guess9 <- renderText("GUESS")
  output$guess10 <- renderText("GUESS")
  output$guess11<- renderText("GUESS")
  output$guess12<- renderText("GUESS")
  output$guess13<- renderText("GUESS")
  output$guess14<- renderText("GUESS")
  
  output$over5 <- renderText("is a global state machine that currently uses a similar type of consensus algorithm as")
  output$over6 <- renderText("known as Proof of Work. ")
  output$over7 <- renderText("is much more interesting because it is Turing Complete which allows arbitrary programs to be run on the blockchain, allowing developers full expressiveness.   

With 
")
  output$over8 <- renderText(", what are known as smart contracts can be built and these allow us to transition to a world where code is law, allowing more predictability and automatic adjudication from our contracts and legal system.  For example on")
  output$over9 <- renderText(" a platform called Uniswap run has shown autonomous code in the cloud can replace highly skilled traditional market makers.  Each new wave of crypto growth brings another competitor, and the new project ")
  output$over10 <- renderText("has a bunch of hype because of its ability to compete with modern day financial platforms like VISA that conduct 60,000 transactions per second.  ")
  output$over11 <- renderText(" is criticized for being more centralized because instead of a decentralized process used to validate the network for consensus they use a system called Proof of Stake and Proof of History.  They won't be alone though because ")
  output$over12 <- renderText(" will soon be moving over from Proof of Work to Proof of Stake.   

Decentralized consensus is good to remove the power of anyone to corrupt a network but is a very difficult mechanism to scale.  
")
  output$over13 <- renderText("is becoming very congested and the transaction fees are quickly pricing out most users and so many competitors like ")
  output$over14 <- renderText(" are now coming up which are able to get much higher throughput with significantly lower fees.  It will be an interesting race and is certainly not a zero-sum game but ")
  output$over15 <- renderText("is currently the biggest network with a market cap just under 1 trillion dollars.  
")
  
  #Initialization

  
  onclick("guess1", {output$guess1 <- renderText("Bitcoin")})
  onclick("guess2", {output$guess2 <- renderText("Bitcoin")})
  onclick("guess3", {output$guess3 <- renderText("Bitcoin")})
  onclick("guess4", {output$guess4 <- renderText("Ethereum")})
  onclick("guess5", {output$guess5 <- renderText("Bitcoin")})
  onclick("guess6", {output$guess6 <- renderText("Ethereum")})
  onclick("guess7", {output$guess7 <- renderText("Ethereum")})
  onclick("guess8", {output$guess8 <- renderText("Ethereum")})
  onclick("guess9", {output$guess9 <- renderText("Solana")})
  onclick("guess10", {output$guess10 <- renderText("Solana")})
  onclick("guess11", {output$guess11 <- renderText("Ethereum")})
  onclick("guess12", {output$guess12 <- renderText("Ethereum")})
  onclick("guess13", {output$guess13 <- renderText("Solana")})
  onclick("guess14", {output$guess14 <- renderText("Bitcoin")})
  
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)
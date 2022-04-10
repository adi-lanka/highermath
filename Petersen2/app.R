#Replace this line with the folder name of the app
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
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
header <- dashboardHeader(title = "Petersen's graph",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
           column(width=4,
                  tableOutput("vertex"),
                  tableOutput("edge"),
                  actionBttn("hamwalk","Hamiltonian Walk"),
                  actionBttn("euler", "Eulerian Cycle"),
                  actionBttn("colorbtn","Vertex Colors"),
                  radioButtons(inputId = "colorV", label = "Color choices",
                               choices = c("Grey", "Beige", "Pink"))
                  
           ),
           column(width = 6,
                  plotOutput("plot1", click = "plot_click", height = 700),
                  h2("Graph Status:"),
                  uiOutput("sameColor")
           )       
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other 

makeVertexDF <- function() {
  DF <- data.frame(V = character(10), x = numeric (10), y = numeric(10), bg = character(10))
  DF$V <- c("RGP","RYB","YGP","RGB","YBP","YGB","BGP","RBP","RYP","RYG")
  DF$x[1:5] <- 2*sin((0:4)*2*pi/5)
  DF$y[1:5] <- 2*cos((0:4)*2*pi/5)
  DF$x[6:10] <- sin((0:4)*2*pi/5)
  DF$y[6:10] <- cos((0:4)*2*pi/5)
  DF$bg <- rep("white",10)
  return(DF)
}

PeteDF <- makeVertexDF()


makeEdgeDF <- function() {
  DF <- data.frame(V1 = character(15), V2 = character(15), color = character(15))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green","blue","purple","red")
  return(DF)
}

edgeDF <- makeEdgeDF()

get.vertex.clicked <- function(x, y){
  PeteDF <- makeVertexDF()
  r <- 0.34
  r2 <- r^2
  for(i in c(1:10)){
    d2 <- (x-PeteDF$x[i])^2 + (y-PeteDF$y[i])^2
    if(d2 < r^2){
      return(PeteDF$V[i])
    }
  }
  return("Not a vertex")
}

mode <- "none"
current.vertex <- "None"

server <- function(session, input, output) {
  plotVertices <- function(DF) {
    par (mar = c(0,0,0,0))
    plot(DF$x,DF$y, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), asp = 1, pch = 21, cex = 10, bg = DF$bg )
    text(DF$x,DF$y,DF$V, cex = 1.5)
  }
  
  plotEdges <- function(vDF,eDF) {
    end <- ifelse(mode == "euler", 20, 15)
    for (i in 1:end){
      v1 <- eDF[i,1]
      v2 <- eDF[i,2]
      color <- eDF[i,3]
      x1 <- vDF[which.max(vDF$V == v1),2]
      y1 <- vDF[which.max(vDF$V == v1),3]
      x2 <- vDF[which.max(vDF$V == v2),2]
      y2 <- vDF[which.max(vDF$V == v2),3]
      segments(x1,y1,x2,y2,col = color, lwd = 2, lty = 3)
    }
  }
  
  regenerate.graph <- function(){
    PeteDF <<- makeVertexDF()
    edgeDF <<- makeEdgeDF()
    output$vertex <- renderTable(PeteDF)
    output$edge <- renderTable(edgeDF)
    output$plot1 <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
    output$sameColor <- renderText("")
    current.vertex <<- "None"
    ham.counter <<- 0
    euler.counter <<- 0
  }
  regenerate.graph()
  
  color.graph <- function(i){
    change.color <- TRUE
    for (j in 1:15){
      v1 <- edgeDF$V1[j]
      v2 <- edgeDF$V2[j]
      if(v1 == PeteDF$V[i]){
        if(PeteDF$bg[which.max(PeteDF$V == v2)] == input$colorV){
          output$sameColor <- renderText("Invalid color")
          change.color <- FALSE
        }
      }
      if(v2 == PeteDF$V[i]){
        if(PeteDF$bg[which.max(PeteDF$V == v1)] == input$colorV){
          output$sameColor <- renderText("Invalid color")
          change.color <- FALSE
        }
      }
    }
    if(change.color){
      #if(PeteDF$bg[i] == "White"){
      #  counter <<- counter + 1
      #}
      output$sameColor <- renderText("")
      PeteDF$bg[i] <<- input$colorV
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdges(PeteDF,edgeDF)
      })
      done <- TRUE
      for(j in c(1:10)){
        if(PeteDF$bg[j] == "white"){
          done <- FALSE
        }
      }
      #if(counter == 10){
      #  print("Done!")
      #}
      if(done){
        # print("Done!")
        output$sameColor <- renderText("Graph successfully colored!")
      }
    }
  }
  
  hamiltonian <- function(i){
    change.color <- FALSE
    output$sameColor <- renderText("")
    if(current.vertex == "None"){
      change.color <- TRUE
    } else {
      for (j in 1:15){
        v1 <- edgeDF$V1[j]
        v2 <- edgeDF$V2[j]
        if(v1 == PeteDF$V[i] && v2 == current.vertex){
          if(PeteDF$bg[which.max(PeteDF$V == v1)] == "white"){
            change.color <- TRUE
          } else {
            output$sameColor <- renderText("Invalid choice, selected same vertex twice!")
            break
          }
        }
        if(v2 == PeteDF$V[i] && v1 == current.vertex){
          if(PeteDF$bg[which.max(PeteDF$V == v2)] == "white"){
            change.color <- TRUE
          } else {
            output$sameColor <- renderText("Invalid choice, selected same vertex twice!")
            break
          }
        }
      }
    }
    
    if(change.color){
      ham.counter <<- ham.counter + 1
      current.vertex <<- PeteDF$V[i]
      PeteDF$bg[i] <<- "yellow"
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdges(PeteDF,edgeDF)
      })
      if(ham.counter == 10){
        # print("Done!")
        output$sameColor <- renderText("Hamiltonian Walk completed!")
      }
    }
  }
  
  eulerCycle <- function(i) {
    if(current.vertex == "None"){
      current.vertex <<- PeteDF$V[i]
      return(0)
    }
    ei <- -1
    for (j in 1:20){
      v1 <- edgeDF$V1[j]
      v2 <- edgeDF$V2[j]
      if(v1 == PeteDF$V[i] && v2 == current.vertex){
        ei <- j
        break
      }
      if(v2 == PeteDF$V[i] && v1 == current.vertex){
        ei <- j
        break
      }
    }
    if(ei < 1){
      print("No Edge!")
      return(0)
    }
    if(edgeDF$color[ei] == "yellow"){
      print("Duplicate Edge!")
    } else {
      edgeDF$color[ei] <<- "yellow"
      euler.counter <<- euler.counter + 1
      current.vertex <<- PeteDF$V[i]
      if(euler.counter == 20){
        output$sameColor <- renderText("Eulerian Walk Completed!")
      }
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdges(PeteDF,edgeDF)
      })
    }
  }
  
  observeEvent(input$plot_click, {
    # print(input$plot_click$x)
    print(get.vertex.clicked(input$plot_click$x, input$plot_click$y))
    v <- get.vertex.clicked(input$plot_click$x, input$plot_click$y)
    if(v == "Not a vertex"){
      print(v)
    } else {
      i <- which.max(PeteDF$V == v)
      if(mode == "color"){
        color.graph(i)
      }
      if(mode == "hamiltonian"){
        hamiltonian(i)
      }
      if(mode == "euler"){
        eulerCycle(i)
      }
    }
  })
  # (RGP,RYG),(RYG,RGB),(RGB,RBP),(YGP,BGP),(RYB,YGB)
  observeEvent(input$euler, {
    regenerate.graph()
    addEdges <- data.frame(V1 = character(5), V2 = character(5), color = character(5))
    addEdges$V1 <- c("RGP","YBP","RGB","YGP","RYB")
    addEdges$V2 <- c("RYG","RYP","RBP","BGP","YGB")
    addEdges$color <- c("black","black","black","black","black")
    edgeDF <<- rbind(edgeDF, addEdges)
    output$plot1 <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
    mode <<- "euler"
  })
  observeEvent(input$colorbtn, {
    regenerate.graph()
    mode <<- "color"
  })
  observeEvent(input$hamwalk, {
    regenerate.graph()
    mode <<- "hamiltonian"
  })
}

#Run the app
shinyApp(ui = ui, server = server)
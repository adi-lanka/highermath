#Replace this line with the folder name of the app
library(shiny)
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
      tableOutput("edge")
    ),
    column(width = 6,
      plotOutput("plot1", height = 700),
      uiOutput("vertexCoord"),
      plotOutput()
    ),
    column(width = 6,
           actionBttn("colorbtn","Vertex Colors"),
           radioButtons(inputId = "colorV", label = "Color choices",
                        choices = c("Grey", "Beige", "Pink")),
           uiOutput("col")
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




makeEdgeDF <- function() {
  DF <- data.frame(V1 = character(15), V2 = character(15), color = character(15))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green","blue","purple","red")
  return(DF)
}






server <- function(session, input, output) {
  plotVertices <- function(DF) {
    par (mar = c(0,0,0,0))
    plot(DF$x,DF$y, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), asp = 1, pch = 21, cex = 10, bg = DF$bg )
    text(DF$x,DF$y,DF$V, cex = 1.5)
  }
  plotEdges <- function(vDF,eDF) {
    for (i in 1:15){
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
  PeteDF <- makeVertexDF()
  edgeDF <- makeEdgeDF()
  output$vertex <- renderTable(PeteDF)
  output$edge <- renderTable(edgeDF)
  output$plot1 <- renderPlot({
    plotVertices(PeteDF)
    plotEdges(PeteDF,edgeDF)
  })
  output$vertexCoord <- renderText({paste0("x=",(input$plot_click$x))})
}

#Run the app
shinyApp(ui = ui, server = server)
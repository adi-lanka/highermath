#GroupD6
source("buttonrows.R")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# library(rlang)
ui <- dashboardPage(
    dashboardHeader(title = "Group D6"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(width=6,
                   box(
                       width = NULL,
                       height = 450,
                       h3 ("Elements of the group"),
                       h4("The identity"),
                       controlRow1(
                           "ctrli"
                       ),  #agb
                       h4("Order 6 elements (rotations)"),
                       controlRow2(
                           c("ctrlr", "ctrlr5")
                       ),  #agb
                       h4("Order 3 elements (even rotations)"),
                       controlRow2(
                           c("ctrlr2", "ctrlr4")
                       ),  #agb
                       h4("Order 2 elements (flips and one rotation)"),
                       controlRow3(
                           c("ctrlr3", "ctrls", "ctrlsr2")
                       ),  #agb
                       controlRow4(
                           c("ctrlsr4", "ctrlv", "ctrlvr2", "ctrlvr4")
                       )
                       
                   ),#box
                   box(
                       width = NULL,
                       height = 150,
                       title = "Subgroups",
                       buttonRow4(
                           inputIds = c("btnC6", "btnC3", "btnD3", "btnV4"),
                           labels = c("Show C6", "Show C3", "Show D3", "Show V4"),
                           btnStyle = "padding:4px;font-size:150%"
                       )  #agb
                   ),#box
                   box(
                       width = NULL,
                       ehight = 150,
                       title = "Cosets",
                       buttonRow2(
                           inputIds = c("btnLC", "btnRC"),
                           labels = c("Show Left Coset", "Show Right Coset"),
                           btnStyle = "padding:4px;font-size:150%"
                       )
                   )
                   
            ),  #col
            column(
                width = 6,
                box(
                    width = NULL,
                    h3("Inputs and Products"),
                    height = 350,
                    htmlOutput("results"),
                    tags$head(tags$style("#results{color:red; font-size:20px;
                    font-style:italic; overflow-y:scroll;
                    max-height: 300px; background: ghostwhite;}")),
                ),
                box(
                    width = NULL,
                    ehight = 150,
                    title = "Conjugate",
                    buttonRow2(
                        inputIds = c("btnmark", "btnconj"),
                        labels = c("Mark Conjugate", "Conjugate"),
                        btnStyle = "padding:4px;font-size:150%"
                    )
                ),
                box(
                    width = NULL,
                    ehight = 150,
                    title = "Generators",
                    buttonRow3(
                        inputIds = c("btnmarkgena", "btnmarkgenb", "btngen"),
                        labels = c("Mark A", "Mark B", "Generate"),
                        btnStyle = "padding:4px;font-size:150%"
                    )
                ),
                box(width = NULL, height = 60,actionBttn("reset", "Clear Inputs and Products") )
            )
            
        ),  #fluid
        fluidRow(
            column(
                width = 10,
                box(width = NULL,
                    height = 415,
                    h3("Multiplication Table"),
                    tableOutput("multable")
                )
            )
        )
    )
    #body
)

source("permutecalc.R")
source("d6calc.R")
source("D6DF.R")
#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
    result <- "ABCDEF"
    for (i in 1:length(v)){
        result <- D6.apply(ifelse(v[i]=="a",a,b), result)
    }
    idx <- which.max(BiggsDF$cfg == result)
    result <- BiggsDF$name[idx]
    return (result)
}
#evaluate(c("a","b"),"(123)","(12)")



#Everything that follows involves something in the UI
server <- function(input, output, session) {
    #Global variables accessible to server()
    N <- 12
    neutral <- "gray90"
    D6DF <- makeD6data(neutral)
    #Elements in the chosen subgroup
    subgroup <- numeric(0)
    #Color for subgroup buttons
    subcolor <- "yellow"
    #Output to display in the text box
    result.list <- ""
    #Result of all multiplications so far
    #problem?
    product <- "ABCDEF"
    
    #Variables for cosets and conjugate subgroups
    conjugating <- FALSE
    generating <- 0
    a <-"I"
    gena <- "I"
    genb <- "I"
    
    # color.list <- c("pink", "lightblue", "purple", "green", "red", "blue")    #colors for cosets
    color.list <- c("paleturquoise", "palegreen", "lavender", "lavenderblush",
                    "peachpuff", "lightyellow", "lightcyan", "mintcream",
                    "lightsalmon", "lightblue", "thistle")
    
    
    displayButton = function(i) {
        renderUI({actionButton(D6DF[i,1],D6DF[i,2],
                               style=paste("padding:4px;
                   font-size:180%;background:",D6DF[i,3]))}) 
    }
    #show all the buttons
    showButtons <- function() {
        output$ctrli <- displayButton(1)
        output$ctrlr <- displayButton(2)                                     
        output$ctrlr2 <- displayButton(3)
        output$ctrlr3 <- displayButton(4)
        output$ctrlr4 <- displayButton(5)
        output$ctrlr5 <- displayButton(6)
        output$ctrls <- displayButton(7)
        output$ctrlsr2 <- displayButton(8)                                     
        output$ctrlsr4 <- displayButton(9)
        output$ctrlv <- displayButton(10)
        output$ctrlvr2 <- displayButton(11)
        output$ctrlvr4 <- displayButton(12)
    }
    showButtons()
    #Display the multiplication table
    multiply <- function(a, b){
        idx <- which.max(BiggsDF$name == a)
        config <- BiggsDF$cfg[idx]
        result <- D6.apply(b,config)
        idx <- which.max(BiggsDF$cfg == result)
        return(BiggsDF$name[idx])
    }
    
    tbl <- outer(D6DF[,2],D6DF[,2],Vectorize(multiply,c("a","b")))
    colnames(tbl) <- D6DF[,2]
    rownames(tbl) <- D6DF[,2] 
    output$multable <- renderTable(tbl,rownames = TRUE)
    #Multiplies by a specified permutation and displays all calculations so far
    compute.and.show <- function(perm){
        if (conjugating) {
            a <<- perm
            output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
            conjugating <<- FALSE
            return()
        }
        if (generating==1) {
            gena <<- perm
            output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
            return()
        }
        if (generating==2) {
            genb <<- perm
            output$genmsg <- 
                renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
            return()
        }
        product <<- D6.apply(perm, product)
        idx <- which.max(BiggsDF$cfg == product)
        result <- BiggsDF$name[idx]
        line.out <- paste(perm,product,result,sep = "&emsp;")
        result.list <<- paste(result.list, line.out, sep = "<br/>")
        output$results<-renderUI(HTML(result.list))
    }
    #Marks all elements in a subgroup with a color
    mark.subgroup <- function() {
        for (i in 1:N){
            D6DF[i,3] <<- ifelse(i %in% subgroup,subcolor,neutral)
        }
        
    }
    #Event handlers for all the element buttons 
    observeEvent(input$btni,{compute.and.show("i")})
    observeEvent(input$btnr,{compute.and.show("r")})
    observeEvent(input$btnr2,{compute.and.show("r2")})
    observeEvent(input$btnr3,{compute.and.show("r3")})
    observeEvent(input$btnr4,{compute.and.show("r4")})
    observeEvent(input$btnr5,{compute.and.show("r5")})
    observeEvent(input$btns,{compute.and.show("s")})
    observeEvent(input$btnsr2,{compute.and.show("sr2")})
    observeEvent(input$btnsr4,{compute.and.show("sr4")})
    observeEvent(input$btnv,{compute.and.show("v")})
    observeEvent(input$btnvr4,{compute.and.show("vr4")})
    observeEvent(input$btnvr2,{compute.and.show("vr2")})
    #The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        #product <<- "ABCDEF"
        output$results<-renderUI(HTML(result.list))
    })
    #Event handlers for the subgroup buttons
    observeEvent(input$btnC6,{
        subgroup <<- c(1,2, 3, 4, 5, 6)
        mark.subgroup()
        showButtons()
    })
    observeEvent(input$btnC3,{
        subgroup <<- c(1, 3, 5)
        mark.subgroup()
        showButtons()
    })
    observeEvent(input$btnD3,{
        subgroup <<- c(1, 3, 5, 7, 8, 9)
        mark.subgroup()
        showButtons()
    })
    observeEvent(input$btnV4,{
        subgroup <<- c(1,4, 7, 10)
        mark.subgroup()
        showButtons()
    })
    #Event handler for left cosets
    observeEvent(input$btnLC,{
        if(identical(subgroup, numeric(0))){
            showButtons()
        } else { 
            
          mark.subgroup()
          idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(D6DF$color == neutral) >0)){
            #Find the first unassigned group element
            in.coset <- which(D6DF$color == neutral)[1]
            in.coset <- apply(D6DF[in.coset, 2], "ABCDEF")
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
                if(j %in% subgroup) {
                    element <- D6.apply(D6DF[j,2], in.coset)
                    idx <- which.max(BiggsDF$cfg == element)
                    element <- BiggsDF$name[idx]
                    #element <- Perm.multiply()
                    k <- which(D6DF[,2] == element)[1]
                    D6DF[k,3] <<- color.list[idx]
                }
            }
            idx <- idx + 1
        }
        showButtons()
        }
    })
    #Right cosets work the same way
    observeEvent(input$btnRC,{
        #if(is.empty(subgroup)){
        #  return()
        #}
        mark.subgroup()
        idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(D6DF$color == neutral) >0)){
            #Find the first unassigned group element
            in.coset <- which(D6DF$color == neutral)[1]
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
                if(j %in% subgroup) {
                    #config <- D6.apply(D6DF[j,2], "ABCDEF")
                    element <- D6.apply(D6DF[in.coset,2], config)
                    idx <- which.max(BiggsDF$cfg == element)
                    element <- BiggsDF$name[idx]
                    k <- which(D6DF[,2] == element)[1]
                    D6DF[k,3] <<- color.list[idx]
                }
            }
            idx <- idx + 1
        }
        showButtons()
    })
    observeEvent(input$btnmark,{
        conjugating <<- TRUE
        output$conjmsg <- renderUI("Click the button for the desired element a")
    })
    observeEvent(input$btnmarkgena,{
        generating <<- 1
        D6DF[,3] <<- rep(neutral,N)
        showButtons()
        output$genmsg <- renderUI("Click the button for generator a")
    })
    observeEvent(input$btnmarkgenb,{
        generating <<- 2
        D6DF[,3] <<- rep(neutral,N)
        showButtons()
        output$genmsg <- renderUI("Click the button for generator b")
    })
    #Generate random sequences of generators.
    #If we generate more than half the group, it's the entire group
    #This algorithm could turn out to be inefficient,and in principle it can fail
    observeEvent(input$btngen,{
        subgroup <<-  numeric(0)
        for (j in 1:(4*N)) {
            v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
            element <- evaluate(v,gena,genb)
            k <- which(D6DF[,2] == element)[1]
            if(!(k %in% subgroup)){
                subgroup <<- c(subgroup,k)
                D6DF[k,3] <<- subcolor
            }
            #If subgroup has more than N/2 elements, it's the entire group
            if (length(subgroup) > N/2){
                subgroup <<- 1:N
                break
            } 
        }  
        mark.subgroup()
        showButtons()
        output$genmsg <- 
            renderUI(paste0("The subgroup generated by ",gena," and ", genb," is now yellow"))
    })
    observeEvent(input$btnclear,{
        subgroup <<- numeric(0)
        generating <<- 0
        gena <<- "I"
        genb <<- "I"
        mark.subgroup()
        showButtons()
        output$genmsg <- renderUI("")
    })
    observeEvent(input$btnconj,{
        aInv <- Perm.inverse(a)
        D6DF[,3] <<- rep(neutral,N)
        for (j in 1:N) {
            if (j %in% subgroup){
                element <- Perm.conjugate(a,D6DF[j,2])
                k <- which(D6DF[,2] == element)[1]
                D6DF[k,3] <<- "pink"
            }
        }
        showButtons()
        output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)






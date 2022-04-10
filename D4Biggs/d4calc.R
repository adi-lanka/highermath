# square
#d4calc.R - Symmetries of the square

D4.makeDataFrame <- function() {
    DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
    DF[1,] <- c("i","ABCD")
    DF[2,] <- c("r","DABC")
    DF[3,] <- c("s","BCDA")
    DF[4,] <- c("t","CDAB")
    DF[5,] <- c("x","CBAD")
    DF[6,] <- c("y","ADCB")
    DF[7,] <- c("z","BADC")
    DF[8,] <- c("w","DCBA")
    return(DF)
}
BiggsDF <- D4.makeDataFrame()
BiggsDF
#a is one of the Biggs symbols for an operation.

D4.showConfigs <- function(DF) {
    par(mar=c(1,1,1,1))
    plot(NULL,xlim=c(0,24),ylim = c(-1,3), asp = 1, axes = FALSE)
    for (i in 0:7) {
        points(c(0,2,2,0,0)+3*i,c(0,0,2,2,0),type = "l")
        lbl <- strsplit(DF[i+1,2],"")[[1]]
        text(c(0.25,1.75,1.75,0.25)+3*i,c(1.75,1.75,0.25,0.25),lbl)
        text(1+3*i,-0.5,DF[i+1,1])
    }
    segments(c(11.75,14.75,19,20.75),c(-0.25,2.25,-0.25,1),
             c(14.25,17.25,19,23.25),
             c(2.25,-0.25,2.25,1),lty = 2)
}
D4.showConfigs(BiggsDF)

#cfg is a string of symbols, reading clockwise from the top of the triangle
#TODO square, similar to showConfigs
D4.showSquare <- function(cfg){
    par(mar=c(1,1,1,1))
    plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
    points(c(0,2,2,0,0),c(2,2,0,0,2),type = "l", lwd = 2)
    lbl <- strsplit(cfg,"")[[1]]
    text(c(0.3,1.7,1.7,0.3),c(1.7,1.7,0.3,0.3),lbl)
}
D4.showSquare("BCAD")
#The return value is the new configuration
D4.apply <- function(a,cfg){
    v <-strsplit(cfg,"")[[1]]   #select first component of list
    w <- switch(a,
                "i" = v,
                "r" = c(v[4],v[1],v[2],v[3]),
                "s" = c(v[2],v[3],v[4],v[1]),
                "t" = c(v[3],v[4],v[1],v[2]),
                "x" = c(v[3],v[2],v[1],v[4]),
                "y" = c(v[1],v[4],v[3],v[2]),
                "z" = c(v[2],v[1],v[4],v[3]),
                "w" = c(v[4],v[3],v[2],v[1])
    )
    s <- paste(w,collapse="") 
    return(s)
}
D4.apply("r","BCAD")

D4.multiply <- function(DF,a,b){
    #Look up the name, which occurs once and only once
    idx <- which.max(DF$name==b)
    #Find the corresponding configuration
    cfg <- DF$cfg[idx]
    #Apply the group operation to it
    newcfg <- D4.apply(a,cfg)
    print(cfg)
    print(newcfg)
    # Look up the configuration
    idx <- which.max(DF$cfg==newcfg)
    return (DF$name[idx])
}
D4.multiply(BiggsDF,"s","t")

#To use this with outer() we must vectorize it
vD4.multiply <- Vectorize(D4.multiply,c("a","b"))
outer(c("i","r","s","t", "x","y","z", "w"),c("i","r","s","t", "x","y","z", "w"),"vD4.multiply", DF = BiggsDF)

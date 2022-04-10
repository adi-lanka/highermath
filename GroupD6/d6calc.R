# hexagon
#d6calc.R - Symmetries of the square
#orientation is counter-clockwise
#r is a 60degree rotation
D6.makeDataFrame <- function() {
    DF <- data.frame(name=rep("",8),cfg=rep("",8),stringsAsFactors = FALSE)
    DF[1,] <- c("i","ABCDEF")
    DF[2,] <- c("r","FABCDE")
    DF[3,] <- c("r^2","EFABCD")
    DF[4,] <- c("r^3","DEFABC")
    DF[5,] <- c("r^4","CDEFAB")
    DF[6,] <- c("r^5","BCDEFA")
    DF[7,] <- c("s","FEDCBA")
    DF[8,] <- c("sr^2","DCBAFE")
    DF[9,] <- c("sr^4","BAFEDC")
    DF[10,] <- c("v","AFEDCB")
    DF[11,] <- c("vr^2","EDCBAF")
    DF[12,] <- c("vr^4","CBAFED")
    
    return(DF)
}
BiggsDF <- D6.makeDataFrame()
BiggsDF
#a is one of the Biggs symbols for an operation.
#TODO draw a square

D6.showConfigs <- function(DF) {
    par(mar=c(1,1,1,1))
    plot(NULL,xlim=c(-1,45),ylim = c(-1,3), asp = 1, axes = FALSE)
    for (i in 0:11) {
        points(c(1/2,3/2,2,3/2,1/2,0,1/2)+4*i,c(sqrt(3),sqrt(3),sqrt(3)/2,0,0,sqrt(3)/2,sqrt(3)),type = "l", lwd = 2)
        lbl <- strsplit(DF[i+1,2],"")[[1]]
        text(c(1/2-1/3,3/2+1/3,2+1/3,3/2+1/3,1/2-1/3,0-1/3)+4*i,c(sqrt(3)+1/3,sqrt(3)+1/3,sqrt(3)/2,0-1/3,0-1/3,sqrt(3)/2),lbl)
        text(1+4*i,-0.5,DF[i+1,1])
    }
    segments(c(25,28,32,35.75,40.25,44.25), 
             c(-0.25, 5*sqrt(3)/6, sqrt(3)/6, sqrt(3)/2, -sqrt(3)/4, 5*sqrt(3)/4),
             c(25,30,34,38.25,41.75,45.75), 
             c(sqrt(3) + 0.25, sqrt(3)/6, 5*sqrt(3)/6, sqrt(3)/2, 5*sqrt(3)/4, -sqrt(3)/4), 
             lty = 2)
    #segments_without_offset = [((1, -0.25),(1, 0.25), "up"), ((),(),"across")]
}
D6.showConfigs(BiggsDF)

#cfg is a string of symbols, reading clockwise from the top left of hexagon
#TODO hexagon, similar to showConfigs
#hex_coords = [(1/2, 2),(3/2, 2),(2, sqrt(3)/2),(3/2, 0),(1/2, 0),(0, sqrt(3)/2)]
D6.showHex <- function(cfg){
    par(mar=c(1,1,1,1))
    plot(NULL,xlim=c(0,3),ylim = c(-1,2), asp = 1, axes = FALSE)
    points(c(1/2,3/2,2,3/2,1/2,0,1/2),c(sqrt(3),sqrt(3),sqrt(3)/2,0,0,sqrt(3)/2,sqrt(3)),type = "l", lwd = 2)
    lbl <- strsplit(cfg,"")[[1]]
    text(c(0.3,1.7,1.7,0.3),c(1.7,1.7,0.3,0.3),lbl)
    
}
D6.showHex("BCADEF")
#The return value is the new configuration
# DF[1,] <- c("i","ABCDEF")123456
# DF[2,] <- c("r","FABCDE")612345
# DF[3,] <- c("r^2","EFABCD")561234
# DF[4,] <- c("r^3","DEFABC")456123
# DF[5,] <- c("r^4","CDEFAB")345612
# DF[6,] <- c("r^5","BCDEFA")234561
# DF[7,] <- c("s","FEDCBA")654321
# DF[8,] <- c("sr^2","DCBAFE")432165
# DF[9,] <- c("sr^4","BAFEDC")216543
# DF[10,] <- c("v","AFEDCB")165432
# DF[11,] <- c("vr^2","EDCBAF")543216
# DF[12,] <- c("vr^4","CBAFED")321654
D6.apply <- function(a,cfg){
    v <-strsplit(cfg,"")[[1]]   #select first component of list
    w <- switch(a,
                "i" = v,
                "r" = c(v[6],v[1],v[2],v[3],v[4],v[5]),
                "r^2" = c(v[5],v[6],v[1],v[2],v[3],v[4]),
                "r^3" = c(v[4],v[5],v[6],v[1],v[2],v[3]),
                "r^4" = c(v[3],v[4],v[5],v[6],v[1],v[2]),
                "r^5" = c(v[2],v[3],v[4],v[5],v[6],v[1]),
                "s" = c(v[6],v[5],v[4],v[3],v[2],v[1]),
                "sr^2" = c(v[4],v[3],v[2],v[1],v[6],v[5]),
                "sr^4" = c(v[2],v[1],v[6],v[5],v[4],v[3]),
                "v" = c(v[1],v[6],v[5],v[4],v[3],v[2]),
                "vr^2" = c(v[5],v[4],v[3],v[2],v[1],v[6]),
                "vr^4" = c(v[3],v[2],v[1],v[6],v[5],v[4])
    )
    s <- paste(w,collapse="") 
    return(s)
}
D6.apply("r","DCBAFE")

D6.multiply <- function(DF,a,b){
    #Look up the name, which occurs once and only once
    idx <- which.max(DF$name==b)
    #Find the corresponding configuration
    cfg <- DF$cfg[idx]
    #Apply the group operation to it
    newcfg <- D6.apply(a,cfg)
    print(cfg)
    print(newcfg)
    # Look up the configuration
    idx <- which.max(DF$cfg==newcfg)
    return (DF$name[idx])
}
D6.multiply(BiggsDF,"s","r")


#d6DF.R
neutral = "gray90"
makeD6data <- function(neutral) {
    N <- 12
    DF <- data.frame(button=character(N),
                     perm =character(N),color=character(N),stringsAsFactors= FALSE)
    DF[1,] <- c("btni","i",neutral)
    DF[2,] <- c("btnr","r",neutral)
    DF[3,] <- c("btnr2","r2",neutral)
    DF[4,] <- c("btnr3","r3",neutral)
    DF[5,] <- c("btnr4","r4",neutral)
    DF[6,] <- c("btnr5","r5",neutral)
    DF[7,] <- c("btns","s",neutral)
    DF[8,] <- c("btnsr2","sr2",neutral)
    DF[9,] <- c("btnsr4","sr4",neutral)
    DF[10,] <- c("btnv","v",neutral)
    DF[11,] <- c("btnv2","vr2",neutral)
    DF[12,] <- c("btnv4","vr4",neutral)
    return(DF)
}

DF <- makeD6data(neutral)
DF

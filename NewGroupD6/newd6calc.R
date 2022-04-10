neutral = "gray90"
makenewD6data <- function(neutral) {
    N <- 12
    DF <- data.frame(button=character(N),
                     perm =character(N),color=character(N),stringsAsFactors= FALSE)
    DF[1,] <- c("btnI", "I","123456", neutral)
    DF[2,] <- c("btn612345", "612345",neutral)
    DF[3,] <- c("btn561234","561234" ,neutral)
    DF[4,] <- c("btn456123","456123",neutral)
    DF[5,] <- c("btn345612","456123",neutral)
    DF[6,] <- c("btn234561","234561",neutral)
    DF[7,] <- c("btn654321","654321",neutral)
    DF[8,] <- c("btn432165","432165",neutral)
    DF[9,] <- c("btn216543","216543",neutral)
    DF[10,] <- c("btn165432","165432",neutral)
    DF[11,] <- c("btn543216","543216",neutral)
    DF[12,] <- c("btn321654","321654",neutral)
    
    return(DF)
}

DF <- makeD6data(neutral)
DF

# DF[1,] <- c("I","ABCDEF")
# DF[2,] <- c("612345","FABCDE")
# DF[3,] <- c("561234","EFABCD")
# DF[4,] <- c("456123","DEFABC")
# DF[5,] <- c("345612","CDEFAB")
# DF[6,] <- c("234561","BCDEFA")
# DF[7,] <- c("654321","FEDCBA")
# DF[8,] <- c("432165","DCBAFE")
# DF[9,] <- c("216543","BAFEDC")
# DF[10,] <- c("165432","AFEDCB")
# DF[11,] <- c("543216","EDCBAF")
# DF[12,] <- c("321654","CBAFED")
#Applies matrix A to a vector from subspace idx
#Looks up the result to find what subspace it is in.
source("F4calc.R")
source("permutecalc.R")
Transform <- function(A,idx){
    m <- makeVecList()
    v <- m[3*idx-2,]   #second vector (first nonzero one) in subspace idx
    x <- vF4Sum(vF4Prod(v[1],A[,1]),vF4Prod(v[2],A[,2]))
    #Find what row in matrix m amatches vector v
    r <- row.match(x,m)
    return(floor((r+2)/3))
}

to.perm <- function(A){
    #p <- Transform(A, i) for i in c(1:5)
    p <- numeric(0)
    for(i in c(1:5)){
        p <- c(p, Transform(A, i))
    }
    
    return(cycle.convert(p))
}


#Frontend:
#TODO: Generate matricies
#TODO: Display results
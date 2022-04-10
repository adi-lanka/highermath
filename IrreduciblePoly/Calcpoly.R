#Irreducible
#TODO: replacement rule
#TODO: polynomial multiplication
#TODO: exponentiation and check

irreducible <- c()

# pol = c(a_2, a_1, a_0)
# a_2 == 1
gen.replacement <- function(pol){
    return(c(5-pol[2], 5-pol[3])%%5)
}


#(x+4)*x = x^2 + 4x = 3x+2 + 4x = (3*1+4)x + 2*1
#rep: x^2 = 3x+2
#curr = c(1, 4)
#rep = c(3, 2)

next.power <- function(curr, rep){
    a_0 <- (curr[1]*rep[2])%%5
    a_1 <- (curr[2] + curr[1]*rep[1])%%5
    return(c(a_1, a_0))
}

print(next.power(c(0, 1), c(2, 1)))

is.gen <- FALSE

gen.powers <- function(pol){
    is.gen <<- TRUE
    rep <- gen.replacement(pol)
    print(rep)
    curr <- c(0, 1)
    res <- curr
    for(i in c(1:24)){
        curr <- next.power(curr, rep)
        print(curr)
        if(curr[1] == 0 && curr[2] == 1 && i < 24){
            is.gen <<- FALSE
        }
        res <- c(res, curr)
    }
    return(matrix(res, ncol=2, byrow=TRUE))
}

print(gen.powers(c(1, 1, 2)))
print(is.gen)
#TODO: subtract one from power for the display

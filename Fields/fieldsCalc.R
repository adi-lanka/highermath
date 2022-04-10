#TODO: Generate dataframe
#TODO: Implement division
#TODO: Implement +,-,* (easy)
#TODO: UI



gen <- matrix(c(1, 2, 1, 1), nrow = 2, byrow = TRUE)
mat <- c(matrix(c(0, 0, 0, 0), nrow = 2, byrow = TRUE), gen)
curr <- gen
for(i in c(2:8)){
    curr <- curr%*%gen
    curr <- curr%%3
    mat <- c(mat, curr)
}

mat <- array(mat, c(2, 2, 9))
mat

# mat[1] = 0
# mat[i] = gen%^%i-1

##
# Input: operation, as a character
#        i = index of M1
#        j = index of M2
calc <- function(op, i, j){
    A <- mat[,,i]
    B <- mat[,,j]
    
    if(op == '+'){
        return ((A+B)%%3)
    }
    if(op == '-'){
        return ((A-B)%%3)
    }
    if(op == '*'){
        return ((A%*%B)%%3)
    }
    if(op == '/'){
        if(j == 1){
            return("DIVIDE BY 0")
        }
        res_i <- (((i-1)-(j-1))%%8)+1
        res_i <- ifelse(res_i-1, res_i, res_i+8)
        print(res_i)
        return(mat[,,res_i])
    }
}

mat[,,2]
mat[,,5]
calc('+', 2, 5)
calc('/', 2, 5)

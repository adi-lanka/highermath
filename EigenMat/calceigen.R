library("pracma")
library("expm")
library("rlang")
eig <- character(0)
    
rake3x3 <- function() {
    repeat{
    c1 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    c2 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    c3 <- sample(c(0:4,0:4, 0:4),3,replace = TRUE)
    A <- cbind(c1,c2)
    A <- cbind(A, c3)
    if (det(A)== 0)
        next
    # if (det(A) <0)
    # {
    #     A <- cbind(c2,c1)
    #     A <- cbind(A, c3)
    # }
    trace <- A[1,1] + A[2,2] + A[3,3]
    print(trace)
    return(A)
    }
}

mat <- rake3x3()
# mat <- c(0,3,2)
# r2 <- c(2,4,2)
# r3 <- c(4,0,1)
# mat <- cbind(mat, r2)
# mat <- cbind(mat, r3)
mat
detm <- det(mat)


trace <- mat[1,1] + mat[2,2] + mat[3,3]
trace
#make maatrices 0-4
charac <- function(lam, tr, det, a) {
    a <- a%^%2
    tr2 <- a[1,1] + a[2,2] + a[3,3]
    #ans <- (lam^3 - tr*lam^2 + 1/2*(tr - tr2*lam - det))
    # == 0
    if(round(lam^3 - tr*lam^2 + 1/2*((tr*tr - tr2)*lam) - det) %%5 == 0) {
        return(lam)
    }
    else {
        return(-1)
    }
}

cayley <- function(tr, det, a) {
    a2 <- a%^%2
    tr2 <- a2[1,1] + a2[2,2] + a2[3,3]
    #ans <- (lam^3 - tr*lam^2 + 1/2*(tr^2 - tr2*lam - det))
    # == 0
    return(round(a%^%3 - tr*(a%^%2) + (1/2)*((tr*tr - tr2)*a) - det*diag(3)))
}

# mat<- mat%%5
mat
for(i in 0:4) {
    #print(i)
    # eg2 <- charac(i, trace, detm, mat)
    # print(eg2)
   if(charac(i, trace, detm, mat) >=0) {
      eig <- c(eig, i)
   }
    # eig.append("four")
}

#eig
if(is_empty(eig)) {
    eig <- "No eigenvalues found"
}
print("Eig: ")
print(eig)
# eigen(mat)

print(cayley(trace, detm, mat))
detm

eig2 <- character(0)
for (i in 0:4) {
  eig2 <- append(eig2, i)
}
eig2

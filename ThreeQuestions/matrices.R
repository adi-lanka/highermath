# one way to initialize a matrix
# populates by column unless you set byrow = TRUE
M <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
M
# can also bind rows and columns
N <- cbind(c(1, 2, 3, 4), c(5, 6, 7, 8), c(9, 10, 11, 12)); N
R <- rbind(c(1, 2, 3, 4), c(5, 6, 7, 8)); R
# more examples of nrow
matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2)
matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE)
# more examples of cbind and rbind
cbind(c(1, 2), c(3, 4), c(5, 6), c(7, 8)) 
rbind(c(1, 3, 5, 7), c(2, 4, 6, 8)) 
# 2nd row
M[2,] 
# 2nd column
M[,2] 
M[1, 2]
# 1st through 2nd rows, 1st and 3rd columns
M[1:2, c(1, 3)] 
# delete 3rd row and 2nd column (useful for dets in 3x3 matrices)
M[-3, -2]
# transpose
t(M)
A <- cbind(c(1, 1), c(1, 2)); A
# inverse of A (useful for row reduction and Axler's method)
solve(A)
# matrix multiplication
A%*%solve(A)
# pointwise application of multiplication
A * solve(A)
# first 1 row of M
head(M, 1)
# sums of rows of M
rowSums(M) 
# sums of columns of M
colSums(M) 
# diagonal nxn matrix with specified values on the diagonals
diag(c(1, 2, 3, 4))
# with just 1 number, this returns an n x n identity matrix
diag(4) 
# vector of values on diagonal
diag(M) 
# how to use Vectorize
samplefunction <- function(x, amazing) {
  if (amazing) {
    return(x + 2)
  }
  else {
    return(x + 1)
  }
}

# name the parameter you want to vectorize (good for mixes of vector/non-vector arguments)
samplevectorizedfunction <- Vectorize(samplefunction, "x"); samplevectorizedfunction
samplefunction(1, TRUE)
samplevectorizedfunction(c(1, 2, 3, 4), TRUE)
add <- function(x, y) {
  return(x + y)
}
addfancy <- function(x, y, awesome) {
  if (awesome) {
    return(x + y)
  }
  else {
    return(x - y)
  }
}
vectorizedaddfancy <- Vectorize(addfancy, "x", "y")
vectorizedaddfancy(c(1, 2, 3, 4), c(4, 5, 6, 7), FALSE)
# apply a function with multiple arguments
mapply(add, c(1, 2, 3, 4), c(-1, -2, -3, -4))
# apply a function with one argument
sapply(1:3, function(x) x^2)

vecfancy <- function(v, awesome = TRUE) {
  return(v[1] + v[2])
}
coordinates <- cbind(c(1, 2), c(3, 4))
c(c(1, 2), c(3, 4))
vectorizedvecfancy <- Vectorize(vecfancy, "v")
vectorizedvecfancy(coordinates)

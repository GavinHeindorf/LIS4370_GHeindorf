require(MASS)

##Creating Matrices
A = matrix(1:100, nrow=10)
B = matrix(1:1000, nrow=10)
  #transpose matrices
At <- t(A)
Bt <- t(B)
  #visually compare
head(A)
head(At)
head(B)
head(Bt)
  #multipling matrices together
A %*% At
At %*% A
B %*% Bt #intersting!
Bt %*% B
  
##Creating Vectors using first row of matrices
a <- A[1,]
b <- B[1,]

a * A
a %*% A
A %*% a

  #function that uses vectors to calculate the product of
    #a matrix and the transposed matrix
matrixTransMultp <- function(matrix){
  if (nrow(matrix) == ncol(matrix)){
    multmatrix <- matrix(nrow = nrow(matrix),
                         ncol = ncol(matrix))
    tm <- t(matrix)
    for (i in 1:nrow(matrix)){
      multmatrix[i,] <- matrix[i,] %*% tm
    }
    return(multmatrix)
  } else{
    paste("Error: Non-square matrix provided")
  }
}

matrixTransMultp(A)
matrixTransMultp(B)

b * B
# does not work: b %*% B
b %*% Bt

##Solving for determint and inverse
det(A) #no inverse possible
det(At)
#does not work: non-square det(B)
  #solve for inverse of non-square matrix
ginv(B)

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will generate a random square matrix that will have 'z' dimensions
## subsequently the inverse of the matrix will be calculated using the solve() function
## the matrices are stored in a list 'cache_mat'

## create empty variables that will be used in the functions
mat <- NULL
inverse <- NULL
true <- NULL
temp <- NULL

makeCacheMatrix <- function(z) {
        if(class(z) == "matrix"){ #check if input argument is a matrix 
                if(!ncol(z) == nrow(z)){ #checks if dimensions are equal, a requirement for square matrices
                        message("Matrix is not a square matrix. Therefore, matrix cannot be inverted.")
                } else {
                        for(i in 1:length(mat)){
                                true[i] <- identical(z, mat[[i]])
                        }
                        if(any(true) == TRUE){ #check if matrix is already stored 
                                message("Matrix is already cached!")
                        } else {
                                mat <<- append(mat, list(z)) #collects all matrices that are created with the function
                                inverse <<- append(inverse, list(solve(z))) #collects all inverse matrices from 'mat'
                                cache <<- list(m=mat, i=inverse) #creates a list with both original matrix and inverse matrix with the same indices 
                        }
                }         
        }
        else{ #if the matrix is new ... collect matrices as previous if/else loop
                mat <<- append(mat, list(matrix(rnorm(z^2),z,z))) 
                inverse <<- append(inverse, list(solve(matrix(rnorm(z^2),z,z))))
                cache <<- list(m=mat, i=inverse)
        }
}


## Write a short comment describing this function
##cacheSolve will take an input matrix and see if it is already stored in the cache
##if it is not, it will calculate the inverse matrix, using the solve() function

cacheSolve <- function(x, ...) {
        for(i in 1:length(cache$m)){ #loops through original matrix list to see if its already cached          
                temp[i] <- identical(x, cache$m[[i]])
        }
        if(any(temp) == TRUE){ #if any of the matrices in temp are already cached, return the stored matix       
                message("getting cache")
                return(cache$i[[which(temp, arr.ind = FALSE, useNames = TRUE)]])
        }
        else { #other, solve the inverse of the matrix
                solve(x)
        }
}

##in order to see if the inverse matrix is correctly calculated,
##multiply the input matrix by the cacheSolve input matrix
##the result should yield an identity matrix 
##e.g., if input matrix is assigned the variable 'e',
##type in console: round(e %*% cacheSolve(e)) 
##round is needed in order to make the very small numbers into 0s



## Caching the Inverse of a Matrix:

## The below function creates a special "matrix" object that can cache its inverse.
## makeVector creates a special "vector", which is really a list containing a function to
## -set the value of the vector
## -get the value of the vector
## -set the value of the mean
## -get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" created by the makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {                         ## if the user has already calculated the same matrix before
        message("getting cached data")
        return(i)                              
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i                                          ## print the inverse matrix 
}

##TESTING THE FUNCTION:
## > source('cachematrix.R')
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > my_matrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > my_matrix <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

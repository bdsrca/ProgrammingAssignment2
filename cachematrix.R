# makeCacheMatrix 
# This function creates a special "matrix" object that can cache its inverse.
# 
# set the value of the matrix
# 
# get the value of the matrix
# 
# set the value of the inverse
# 
# get the value of the inverse
# 

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# 
# The cacheSolve function computes the inverse of the special "matrix" 
#
# returned by makeCacheMatrix above. If the inverse has already been calculated 
#
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getinverse()
    if(!is.null(v)) {
        message("getting the cached matrix")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v
}

# Testing case 1
library("matlib")

A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
Sp_AI <- makeCacheMatrix(A)
cacheSolve(Sp_AI)

AI <- inv(A)
AI

#Test case 2
B <- matrix(c(1,2,3,0,1,5,5,6,0),nrow=3, byrow=TRUE)
Sp_BI <- makeCacheMatrix(B)
cacheSolve(Sp_BI)

BI <- inv(B)
BI

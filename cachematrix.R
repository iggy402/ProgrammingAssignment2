## The purpose of this project is to solve for the inverse
## of a matrix and cache the result. 
## makeCacheMatrix is a matrix object that can store a cached inverse
## cacheSolve solves for the inverse of a makeCacheMatrix object

## A special matrix object that can store its cached inverse
makeCacheMatrix <- function(x = matrix()) {
#The inverse matrix
  matInverseCached <- NULL
#Sets the matrix 
  fnSet <- function(y) {
    x <<- y
    matInverseCached <<- NULL
  }
#Gets the matrix
  fnGet <- function() x
#Retrives the inverse
  fnGetInverse <- function() matInverseCached
#Sets the cached inverse
  fnSetInverse <- function(matInverse) matInverseCached <<- matInverse
#Returns the caching functions
list(fnSet = fnSet, fnGet = fnGet, 
     fnGetInverse = fnGetInverse, fnSetInverse = fnSetInverse)
}

##Returns the inverse matrix for a given
##makeCacheMatrix object.
cacheSolve <- function(x, ...) {
        matInverse <- x$fnGetInverse()
        #Retrieves cached inverse if exists
        if(!is.null(matInverse)) {
          message("getting cached data")
          return(matInverse)
        }
        #Otherwise, calculates the inverse
        data <- x$fnGet()
        matInverse <- solve(data)
        #Caches the inverse matrix
        x$fnSetInverse(matInverse)
        matInverse
}

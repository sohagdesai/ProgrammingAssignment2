## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list object from an input matrix. The list
## object has methods to set and get the input matrix and to get and 
## set the variable that stores the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of a matrix passed into it. If the
## the inverse has not been computed, it computes the inverse and caches the 
## computation. If the inverse has previously been computed, it returns the 
## cached result. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

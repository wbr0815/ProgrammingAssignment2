## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
             x<<- y
             inverse<<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inverse<<- solve
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
        }
        sourcematrix <- x$get()
        inverse <- solve(sourcematrix,...)
        x$setInverse(inverse)
        inverse
}

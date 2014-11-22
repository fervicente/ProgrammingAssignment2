## Defines a matrix object and methods to compute the inverse
## storing the obtained results in cached memory. Only works
## with invertible matrices (no checking is performed). PA2-Rprogramming

## Defines the matrix object, getters and setters
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # init matrix inverse
        
        # Allocate data in matrix object
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        
        # Get data from matrix object
        get <- function() x
        
        # Set inverse value 'i'
        setinverse <- function(inverse) i <<- inverse
        
        # Get inverse value 'i'
        getinverse <- function() i
        
        # List of implemented functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes matrix inversion, if exists gets cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # Get inverse into variable 'i'
        if(!is.null(i)) {
          # if the inverse was already calculated provide cached data
          message("getting cached data")
          return(i)
        }
        
        # if the inverse was not calculated, do it now
        data <- x$get()       # get matrix data
        i <- solve(data, ...) # compute matrix inverse
        x$setinverse(i)       # allocate inverse value for future queries
        i
}

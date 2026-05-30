## Makecachematrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Sets a new matrix and clear the old cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    
    # Retrieves the current matrix
    get <- function() x
    
    # Stores the calculated inverse in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # Retrieves the cached inverse
    getinverse <- function() inv
    
    # Is then able to return a list of the available methods
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cachesolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {
    ## This attempts to retrieve the inverse from the object's cache
    inv <- x$getinverse()
    
    # If the cache is not empty, return the cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the cache is empty, retrieve the raw matrix data
    data <- x$get()
    
    # After, this calculates the inverse using the solve() function
    inv <- solve(data, ...)
    
    # Stores the newly calculated inverse back into the cache
    x$setinverse(inv)
    
    # Finally, this step returns the inverse
    inv
}

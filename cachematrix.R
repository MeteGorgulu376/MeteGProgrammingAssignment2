## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set a new matrix and clear the old cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    
    # Retrieve the current matrix
    get <- function() x
    
    # Store the calculated inverse in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # Retrieve the cached inverse
    getinverse <- function() inv
    
    # Return a list of the available methods
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {
    ## Attempt to retrieve the inverse from the object's cache
    inv <- x$getinverse()
    
    # If the cache is not empty, return the cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the cache is empty, retrieve the raw matrix data
    data <- x$get()
    
    # Calculate the inverse using the solve() function
    inv <- solve(data, ...)
    
    # Store the newly calculated inverse back into the cache
    x$setinverse(inv)
    
    # Return the inverse
    inv
}

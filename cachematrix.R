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

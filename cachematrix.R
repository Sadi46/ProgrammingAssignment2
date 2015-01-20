## Assignment: Caching the Inverse of a Matrix

#### It is assumed that the matrix supplied is always invertible ####

## The first function, makeCacheMatrix creates a special "matrix"
## that can cache its reverse. 
## It is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" 
## created with the above function. If the inverse has already been computed 
## (and the matrix has not changed), then cacheSolve retrieves the inverse from
## the cache and skips the computation (with the message "getting cached data"). 
## Otherwise, it computes the inverse of the matrix and sets the inverse in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Tested and works ##
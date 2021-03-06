## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse by:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        makeCacheMatrix.object <- makeCacheMatrix(x)    
        
        i <- makeCacheMatrix.object$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- makeCacheMatrix.object$get()
        i <- solve(data, ...)
        makeCacheMatrix.object$setinverse(i)
        
        i
}


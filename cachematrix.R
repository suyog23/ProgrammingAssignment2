## There are two functions. The makeCacheMatrix() function returns a list of functions. This list contains individual 
## functions to set and get the matrix which is set. This can also be used to set the inverse of matrix. The second 
## function (cacheSolve()) uses the cached matrix of makeCacheMatrix() and finds its inverse if not already found. 
## Assumption is that the matrix passed to the function is always invertible i.e it is square matrix.

## First function returns a list of four functions. These four functions can be used as follows:
## 1. set(): It can be used to set any matrix. The matrix to be set is passed as argument to this function.
## 2. get(): It can be used to get the matrix which has been set using set() function.
## 3. setInverse(): It can be used to set inverse of any matrix. The argument passed to this should be inverse of the
## required matrix.
## 4. getInverse(): It can be used to get inverse of any matrix which has been set using setInverse() function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y=matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(CacheMatrix=matrix()) m <<- CacheMatrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## CacheSolve() function will cache the matrix set by makeCacheMatrix() function and return the inverse of this matrix.
## If the inverse is already set then it returns the same cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting Cached Inverse Matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly. 
## Below are two functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
        ## x is an invertabile matrix
        ## returns a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse matrix
        ##              4. get the inverse matrix
        ##      must be used as input to cacheSolve()              
        inv <- NULL
        set <- function(y) {
                x <<- y ## assigns value to parent environment
                inv <<- NULL
        }
        
        get <- function()x
        setInvMatrix <- function(inverse) inv <<- inverse
        getInvMatrix <- function() inv
        list(
                set = set, 
                get = get, 
                setInvMatrix = setInvMatrix, 
                getInvMatrix = getInvMatrix
        )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cachSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInvMatrix()
        
        ## if inverse in already calculated
        if (!is.null(inv)){
                ## skip computation, get it from cache 'inv'
                message("get cached data")
                return(inv)
        }
        
        ## otherwise, calculate the inverse matrix
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## set inverse matrix with setInvMatrix function from makeCacheMatrix()
        x$setInvMatrix(inv)
        
        return(inv)
}
## The first function makeCacheMatrix, creates a function that is capable of taking a 
## matrix as input, and returning a list of functions: set matrix, get matrix,
## set the inverse of the input matrix and getting the inverse. 
## this then is used as the input "inverse" matrix for cacheSolve



makeCacheMatrix <- function(x = matrix()) {

        
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cachesolve takes as input the inverse matrix from makeCacheMatrix
## it looks to set if the inverse has been already calculated, ie. from makeCacheMatrix.
## if so, it returns the values of the inverse matrix along with a message
## if not, it calculates the inverse and set the value of the inverse matrix 
## using setinv, storing it in the cache

cacheSolve <- function(x, ...) {
       inv = x$getinv()
        
       
        if (!is.null(inv)){
               
                message("getting cached data")
                return(inv)
        }
        
       
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        
        x$setinv(inv)
        
        return(inv)
}
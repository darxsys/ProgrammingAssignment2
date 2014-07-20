## This function creates the special matrix used for 
## caching matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function either gets already cached inverse
## or calculatest the inverse and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("cached inverse exists. getting it.")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}

## Creates functions to cache inversed matrices and to check whether an 
## inversed matrix already exist

## Create matrix cache for solved/inversed matrices

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Check cache before calling solve, return cached value instead of running
## function. If it doesn't exist, run the function

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m   
}

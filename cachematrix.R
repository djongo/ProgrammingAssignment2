## Creates functions to cache inversed matrices and to check whether an 
## inversed matrix already exist

## Create matrix cache for solved/inversed matrices

makeCacheMatrix <- function(x = matrix()) {
    # set default if cacheSolve hasn't been used yet
    m <- NULL
    
    # define setters and getters
    
    # set value of matrix
    set <- function(y) {
        # caches the matrix given as input
        x <<- y
        # set m to NULL
        m <<- NULL
    }
    
    # get matrix
    get <- function() x
    
    # set inverse matrix
    setmatrix<-function(solve) m<<- solve
    
    # get inverse matrix
    getmatrix<-function() m
    
    # create list for the 4 functions
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Check cache before calling solve, return cached value instead of running
## function. If it doesn't exist, run the function

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # set m to inverse matrix
    m <- x$getmatrix()
    
    # check if we had a match
    if(!is.null(m)) {
        
        # let the user know we got result from cache 
        message("getting cached data")
        
        # return the inverse matrix
        return(m)
    }
    
    # since the if statements uses return if it found something
    # this will be the else part - e.g. didn't find a match
    
    # set data to input matrix
    data <- x$get()
    
    # set m to inverse matrix
    m <- solve(data, ...)
    
    # put inverse matrix in cache
    x$setmatrix(m)
    
    # return the inverse matrix
    m   
}

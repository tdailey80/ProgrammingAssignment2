## The below functions calculate the inverse of a provided matrix using 
##the solve function.  When called the cahced version of the matrix is 
##retreived using lexical scoping.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # sets m to a default value of NULL
    m <- NULL
    set <- function(y) {
        # stores the matrix passed in the function argument in the cache
        x <<- y
        # sets m (ultimately the inverse matrix) to a default value of NULL
        m <<- NULL
    }
    #initializes the list variables
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    # creates a list to store the four cache functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then 
##cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #retrieves the inverse matrix and stores it
    m <- x$getinverse()
    
    # checks to see if there is a matrix in the cached variable m
    if(!is.null(m)) {
        
        # check if the matrix in cache has changed.  
        if(identical(x$setmatrix,x$getmatrix)) {
            message("matrix unchanged, retrieving and returning cached matrix")
            return(m)
        }
        else{
            break #continues to solve if matrix has changed
        }
    }
    
    # if the cached matrix changed then...
    # stores the matrix passed in the function argument
    data <- x$get()
    
    # solves for the inverse of the matrix passed in
    m <- solve(data, ...)
    # caches the inverse functions
    x$setinverse(m)
    # returns the inverse function
    m
}
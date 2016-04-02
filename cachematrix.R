## Put comments here that give an overall description of what your
## functions do
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve #set inverse matrix
     getsolve <- function() m                #get inverse matrix
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)               ## create list of functions 
 }
## computes the inverse of the special "matrix" . іf the inverse has already been calculated (and
## the matrix has not changed), then it should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...){
    m <- x$getsolve() 
    if(!is.null(m)) {
        message("getting cached data")  # sent message indicating this is just cache
        return(m)                       # return the cache  
    }
    data <- x$get()                     # get the matrix used by makeCacheMatrix function 
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

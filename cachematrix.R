## This is a pair of function that cache the inverse of matrix


## This function is used to set the matrix and or set the inverse matrix
## the user also can get the matrix that has been set
## and can get the cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        
        setInverse <- function(invmatrix) m <<- invmatrix
        
        getInverse <- function() m 
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function is used to return the inverse matrix 
## that has been set up in makeCacheMatrix$setInverse()
## or return the inverse matrix of matrix 'x' 

cacheSolve <- function(x, ...) {
        
        # this code is run when the user set setInverse() in makeCacheMatrix function
        
        m <- x$getInverse()
        
        #checking if the user set the inverse matrix or not
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        
        # this code is run if the user haven't setInverse() in makeCacheMatrix function
        # and give the inversed matrix value as the return
        
        data <- x$get()
        m <- solve(data)
        
        x$setInverse(m) #set the inverse matrix
        m               #return inversed matrix
}

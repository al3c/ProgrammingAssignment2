## The first function, makeVector creates a special "matrix"

## this is a matrix containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize matrix inverse
    m_inv <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse
    set_inv <- function(inverse) m_inv <<- inverse
    
    ## get the value of the inverse
    get_inv <- function() m_inv
    
    ## returns a list of the above functions
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 

## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache 
## via the set_inverse function.

cacheSolve <- function(x, ...) {
    ## checking if the inverse of the matrix is already cached
    ## if the inverse is cached, we return the cached inverse
    m_inv <- x$get_inv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    
    ## else, we get the matrix and save it to the data variable
    ## then calculate the inverse and cache it
    data <- x$get()
    m_inv <- inverse(data, ...)
    x$set_inv(m_inv)
   
    ## returns the inverse
    m_inv
    
}

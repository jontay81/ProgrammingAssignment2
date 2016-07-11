# Cachematrix.R contains a function to create a special "matrix" object that
# can cache itself and its inverse. It also contains a function to solve 
# the inverse of the special "matrix" object or retrieve the solution 
# from the cache if it has been previously solved.

# makeCacheMatrix creates a special "matrix" object with functions to 
# cache and retrieve its own value and the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #cache value of self in parent environment
    set <- function(y) {    
        x <<- y
        inv <<- NULL
    }
    
    # return value of self
    get <-  function() x   
    
    # store inverse of self in parent environment
    setInverse <- function(inverse) inv <<- inverse 
    
    #return value of inverse
    getInverse <- function() inv
    
    #return list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


# cacheSolve returns the inverse of x from the cache if it has been previously
# solved. If not, the function solves x, stores the inverse in the cache, and 
# returns the inverse.

cacheSolve <- function(x, ...) {
    
    # retrieve inverse of x from cache
    inv <- x$getInverse()
    
    # if inverse exists in cache return the cached inverse
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    
    # if it doesn't exist, retrieve x, solve the inverse, and cache the value 
    matrix <- x$get()
    
    inv <- solve(matrix)
    
    x$setInverse(inv)
    
    #return the inverse
    inv
}

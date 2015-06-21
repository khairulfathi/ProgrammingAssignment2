# makeCacheMatrix creates a list containing a function 
# 1. Set and get the value of matrix, 
# 2. Set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. Steps are :
# 1. Check if the inverse has already been computed
# 1a. If yes, get the result and skip the computation 
# 1b. If not, compute the inverse, set the value in the cache using setinverse function


cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

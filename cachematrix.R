## makeCacheMatrix : creates a matrix which is able to store its Inverse
# so that it can be computed once for all, and then used as many times as needed 


## Using those two functions together unable us to manipulate matrix whose inverse won't be computed more than once

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(invers) i <<- invers
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve : adaptation of the function 'solve' to our new type of matrix 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
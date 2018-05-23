## This function calculates the inverse of a given matrix, return the result, and caches
## it. If it's found out that the result has already been cached, the function sources 
## and returns it.

## The function creates a list that provides functions to calculate and cache the 
## inverses of matrix x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## the function returns the cached inverse matrix of matrix x, and calculate the inverse  
## matrix if not cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

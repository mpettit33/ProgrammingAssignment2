
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x                             ##gets current matrix
     setinverse <- function(solve) m <<- solve       ##sets inverse of current matrix
     getinverse <- function() m                      ##gets inverse of current matrix
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##set subfunction names for function
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x$getinverse() 
     if(!is.null(m)) {                     ##if no inverse stored in cache
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
        ## Return a matrix that is the inverse of 'x'
}



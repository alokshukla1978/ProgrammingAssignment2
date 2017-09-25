# This function provides cacheMatrix object and associated getter/ 
# setter functions. This could be used to store the cached value
# get() would return the source matrix object
# set() would set the matrix object with input value
# getinverse() would return the stored inverse matrix object
# setinverse() would store the calculated inverse matrix object

makeCacheMatrix <- function(sMatrix = matrix()) {
        rMatrix <- NULL
        set <- function(y) {
          sMatrix <<- y
          rMatrix <<- NULL
        }
        get <- function() sMatrix
        setinverse <- function(revMatrix) rMatrix <<- revMatrix
        getinverse <- function() rMatrix
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculates inverse of matrix. However, it would return
# the cached value if
# 1) a cache value of inverse already exists due to previous request
# 2) the original source matrix has not changed

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        sourceMatrix <- x$get()
        m <- solve(sourceMatrix)
        x$setinverse(m)
        m
}

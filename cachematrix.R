## The following pair of functions cache and compute the 
## inverse of a matrix

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse_matrix <- NULL
     set <- function(y){
          x <<- y
          inverse_matrix <<- NULL
     }
     get <- function () return(x);
     setinverse <- function(solve) inverse_matrix <<- solve;
     getinverse <- function () return(inverse_matrix);
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse
        if(!is.null(inverse_matrix)){
             message("getting inversed matrix")
             return(inverse_matrix)
          }
          data <- x$get()
          inverse_matrix <- solve(data, ...)
          x$setinverse(inverse_matrix)
          inverse_matrix
        ## Return a matrix that is the inverse of 'x'
}

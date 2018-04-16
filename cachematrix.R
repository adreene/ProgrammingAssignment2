## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    
    set <- function(y = matrix ()) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function() inv_matrix <<- solve(x)
    getinverse <- function() inv_matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## above. If the inverse has already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
    if (is.recursive(x))
        inv_matrix <- x$getinverse()
    else
        inv_matrix <- NULL
    
    
    if (!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
  
    new_matrix <- makeCacheMatrix()
    new_matrix$set(x)
    new_matrix$setinverse()
    new_matrix$getinverse()
    inv_matrix <- new_matrix$getinverse()
    inv_matrix
  
}

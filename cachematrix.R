## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL
    set <- function(y){
        x <<- y
        mtx <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) mtx <<- inverse
    getInverse <- function() mtx 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtx <- x$getInverse()
    if(!is.null(mtx)){
        message("getting cached data")
        return(mtx)
    } else {
        message("Don't worry, be happy, data is already cached")
    }
    
    mat <- x$get()
    mtx <- solve(mat,...)
    x$setInverse(mtx)
    mtx
}

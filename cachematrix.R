## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.


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


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtx <- x$getInverse()
    if(!is.null(mtx)){
        message("getting cached data")
        return(mtx)
    } else {
        message("this had not been cached until now")
    }
    
    mat <- x$get()
    mtx <- solve(mat,...)
    x$setInverse(mtx)
    mtx
}


## To test this function you can follow executing this lines:
my_matrix <- makeCacheMatrix(matrix(c(1,3,4,9), 2, 2))

my_matrix$get() #returns de matrix stored in my_matrix

my_matrix$getInverse() #should return NULL

cacheSolve(my_matrix) #should return "this had not been cached until now"

cacheSolve(my_matrix) #by calling this a second time, should return "getting cached data"
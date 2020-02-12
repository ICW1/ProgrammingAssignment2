## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
        ##initialise inverse property
        i <- NULL
        ## method to set matrix
        set <- function( matrix) {
            m <<- matrix
            i <<- NULL
        }
        ## method to get matrix
        get <- function() { m }
        ## method to set inverse of matrix
        setInverse <- function(inverse) { i <<- inverse }
        ## method to get inverse of matrix
        getInverse <- function(inverse) { i }
        ## return list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     ## return matrix that is inverse of x
     i<- x$getInverse()
     ## return the inverse if cahced
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     ## else get the matrix from our object
     data <- x$get()
     ## calculate the inverse from this matrix
     m <- solve(data, ...) %*% data
     ## cache the inverse
     x$setInverse(m)
     ## return the inverse
     m
 }

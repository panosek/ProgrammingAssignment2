## Matrix inversion is usually a costly computation. For large enough matrices it is beneficial to cache
## the inverse of a matrix rather than computing it repeatedly . 
## The pair of functions below cache the inverse of a matrix.



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {


##For example, if X is a square invertible matrix, then solve(X) returns its inverse

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
       
}
## Matrix inversion is usually a costly computation. For large enough matrices it is
## beneficial to cache the inverse of a matrix rather recomputing it. 



## makeCacheMatrix: This function creates and returns a list of functions to:
## set a matrix; get a matrix; set the inverse matrix; get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(){ return(x) }
        
        setinverse <- function(inverse) {inv <<- inverse}
        
        getinverse <- function() {return(inv)}
        
        return(list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse))

}



##cacheSolve: This function computes and returns the inverse of the "matrix" previously passed to 
## or set by makeCacheMatrix above.
## If the inverse was previously calculated it retrieves the inverse from cache.

cacheSolve <- function(x, ...) {

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        return(inv)
       
}

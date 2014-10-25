## Matrix inversion is usually a costly computation. For large enough matrices it is
## beneficial to cache the inverse of a matrix rather recomputing it. 



## makeCacheMatrix: This function takes a matrix as input, caches it
##                  as well as creating and returning a list of functions to:
##
## 1. set/cache the matrix in memory 
## 2. get the matrix
## 3. set/cache the inverse matrix in memory
## 4. get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(){ return(x) }
        
        setinverse <- function(inverse) {inv <<- inverse}
        
        getinverse <- function() {return(inv)}
        
        # return a list of the functions created above with their environments.
        
        return(list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse))

}



##cacheSolve: This function takes as it's input the return value of makeCacheMatrix 
##            and it computes and returns the inverse of the "matrix" previously passed 
##            to or set by makeCacheMatrix above.
##            If the inverse was previously calculated it retrieves the inverse from cache.


cacheSolve <- function(x, ...) {
        
        # check if the inverse was previously cached. If so simply get and return it.
  
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If we got here then the inverse was not cached.  Get the cached matrix, calculate it's
        # inverse using R's predefined solve function, cache it and return it.
        
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        return(inv)
       
}

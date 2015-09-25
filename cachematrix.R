## Matrix inversion is usually a costly computation 
##there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below is a pair of functions that cache the inverse of a matrix.

## First, create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function (y){
                x <<-y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function (inverse) v <<- inverse
        getinverse <- function()v
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## Second, create a function that compute the inverse of the special matrix returned by makeCacheMatrix
## it first examines if the inverse has been calculated and the matrix has not changed
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        v <- x$getinverse()
        if (!is.null(v)){
                message("getting cached matrix")
                return(v)
        }
        matrix <- x$get()
        v <- solve(matrix,...)
        x$setinverse(v)
        v
}

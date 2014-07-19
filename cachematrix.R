## This pair of functions will cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##
## 1  set the value of the matrix
## 2  get the value of the matrix
## 3  set the value of the inverse
## 4  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## The second function, cacheSolve returns invers from the cachi if
## it is calculated before. If no cached inverse then solve,
## store in the cache and return the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
}

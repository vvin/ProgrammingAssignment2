## Togther makeCacheMatrix() and cacheSolve() together implement a caching system to store
## previously computed matrix inverses. This is useful because matrix inversion is
## computationally expensive.

## makeCacheMatrix() takes a matrix as an argument and returns a special type of matrix
## which caches its inverse.
##
## RETURNS: A list of functions (set, get, setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inverse <<- inv
        getinv <- function() m_inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## cacheSolve() computes the inverse of a "matrix object" created by makeCacheMatrix()
##
## RETURNS: the inverse of the matrix passed in

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinv()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data, ...)
        x$setinv(m_inverse)
        m_inverse
}

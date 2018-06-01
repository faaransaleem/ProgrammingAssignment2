
## faaran.saleem EDIT
## The following function creates a matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function computes the inverse of the 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the input matrix is the same
## then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

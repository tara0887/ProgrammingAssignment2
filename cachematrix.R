## The functions below help in reducing the computation of Inverse of a matrix more than once by saving it in a cache ##and returning its value when asked if already solved.


## The below function creates a matrix and stores its values and a variable for its inverse in a list. Inverse value ##is set to NULL initially

makeCacheMatrix <- function(x = matrix()) {
	     m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The below function computes the inverse ans stores it in cache. If asked again it will give the stored value rather ##than computing again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'       
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


## This function computes the inverse of a matrix storing the result in cache.
## In this way future visits to this object if not re-calculate estimated

## is used as an object which has an attribute to store the matrix and
## another to store the inverse matrix. Besides the methods defined to
## access and modify attributes

makeCacheMatrix <- function(x = matrix()) {
				minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setminverse <- function (minverse) minv <<- minverse
        getminverse <- function() minv
        list(set = set, get = get,
             setminverse = setminverse,
             getminverse = getminverse)
}

## This function is responsible for managing whether the calculation
## of the inverse matrix is stored or not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
				minv <- x$getminverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setminverse(minv)
        minv
}

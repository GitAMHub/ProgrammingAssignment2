## ProgrammingAssignment2 for Coursera

# The first function "makeCacheMatrix"creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## "cacheSolve" computes the inverse of the "matrix" returned by "makeCacheMatrix". If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Cache exists, getting it:")
                return(inv)
        }
        else
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

# Exemple with code:
#
# x <- matrix(1:4,2,2)
# mx <- makeCacheMatrix(x)
# mx$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# No cache first time
# cacheSolve(mx)
#       [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5

# Yes cache second time
# cacheSolve(mx)
# Cache exists, getting it:
#       [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5


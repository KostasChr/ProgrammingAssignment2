
## Matrix inversion is usually a costly computation and there may be a benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It receives as an optional argument a matrix and returns 
## a list of functions that are applied on the matrix: 
## -- set: insert new matrix data
## -- get: get the current matrix data
## -- getinv: get the inverse of the matrix
## -- setinv: set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

# Example Run 
# > x = matrix( c(2,4,3,1,4,6,7,4,3), nrow = 3, ncol = 3)
# > result = makeCacheMatrix(x)
#
# > result$get()
# [,1] [,2] [,3]
# [1,]    2    1    7
# [2,]    4    4    4
# [3,]    3    6    3
# 
# > cacheSolve(result)
# [,1]  [,2]        [,3]
# [1,] -0.2  0.65 -0.40000000
# [2,]  0.0 -0.25  0.33333333
# [3,]  0.2 -0.15  0.06666667
# 
# > cacheSolve(result)
# getting cached data
# [,1]  [,2]        [,3]
# [1,] -0.2  0.65 -0.40000000
# [2,]  0.0 -0.25  0.33333333
# [3,]  0.2 -0.15  0.06666667

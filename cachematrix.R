## Create special object to store and set the inverse of a matrix

makeCacheMatrix <- function(matrix = matrix()) {
    inv <- NULL
    set <- function(newMatrix) {
        matrix <<- newMatrix
        inv <<- NULL
    }
    get <- function() matrix
    setInv <- function(newInv) inv <<- newInv
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculate inverse of matrix via caching (if available) or computation

cacheSolve <- function(x, ...) {
    cachedInv <- x$getInv()
    if(!is.null(cachedInv)) {
        message("getting cached data")
        return(cachedInv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInv(inv)
    inv
}

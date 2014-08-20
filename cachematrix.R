## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
    Xinv <- NULL
    set <- function(Y) {
        X <<- Y
        Xinv <<- NULL
    }
    get <- function() X
    setInv <- function(solve) Xinv <<- solve
    getInv <- function() Xinv
    list(set = set, get = get,
    setInv= setInv,
    getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
    Xinv <- X$getInv()
    if(!is.null(Xinv)) {
        message("getting cached data")
        return(Xinv)
    }
    data <- X$get()
    Xinv <- solve(data, ...)
    X$setInv(Xinv)
    Xinv
}

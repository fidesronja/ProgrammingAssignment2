## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        #
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function() X
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        inv <- X$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setInv(inv)
        inv
}

##m<-makeCacheMatrix(matrix(c(7,10,8,2,1,0,6,13,5),nrow = 3, ncol = 3))
##cacheSolve(m)
##cacheSolve(m)
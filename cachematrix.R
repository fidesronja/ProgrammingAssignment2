#The fist function (makeCacheMatrix) creates a special "matrix" object that
##can cache its inverse
##The second function (cacheSolve) computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.


#This function "makeCacheMatrix()" creates a list of functions to
##1.set the value of a matrix (set())
##2. get the value of the matrix (get())
##3. set the value of the inverse of the matrix (setInv())
##4. get the value of the inverse of the matrix (getInv())
#The inverse is initially set to NULL and it's value doesn't change before the
##cacheSolve function is called the first time 
##(you change it by calling X$setInv() but that is not intended)

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL

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


# The following function calculates the inverse of the matrix created with the 
##above function. However, it first checks to see if the inverse has already
##been calculated (i.e. inv does not equal NULL). If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the inveserse of 
##the data and sets the value of the inverse (inv) in the cache via the setInv function.

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

# potential testscrip:
#m<-makeCacheMatrix(matrix(c(7,10,8,2,1,0,6,13,5),nrow = 3, ncol = 3)) 
##nothing is displayed but is assigned a 3by3 matrix 
#cacheSolve(m)
##the inverse of the above matrix is calculated and displayed
#cacheSolve(m)
##the message "getting cached data" is displayed and the previously calculated 
#inverse is display which was stored in m
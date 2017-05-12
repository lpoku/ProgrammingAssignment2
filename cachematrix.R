## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##clears out any stored values for mtx
makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mtx <<- inv
        getinv <- function() mtx
        list(set = set, get=get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##checks for stored inverse
        if(!is.null(mtx)) {
                message("getting cached inverse")
                return(mtx)
        }
        data <- x$get()
        ##calculates inverse stores as mtx
        mtx <- solve(data, ...)
        x$setinv(mtx)
        mtx
}

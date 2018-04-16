## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), dime = 4) {
        ## Write a short comment describing this function
                i <- matrix(NA, dime, dime)
                set <- function(y) {
                        x <<- y
                        i <<- matrix(NA, dime, dime)
                }
                get <- function() x
                setinverse <- function(inverse) i <<- solve
                getinverse <- function() i
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.na(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

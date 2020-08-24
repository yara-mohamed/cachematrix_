## Caching an Inverse of Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inverseMatrix <- NULL
                set <- function(y) {
                        x <<- y
                        inverseMatrix <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inverseMatrix <<- inverse
                getinverse <- function() inverseMatrix
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getinverse()
                if(!is.null(inverseMatrix)) {
                        message("getting cached data")
                        return(inverseMatrix)
                }
                data <- x$get()
                inverseMatrix <- Inverse(data, ...)
                x$setinverse(inverseMatrix)
                inverseMatrix
        }
        ## Return a matrix that is the inverse of 'x'


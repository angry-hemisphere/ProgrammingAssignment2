## Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    ## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }

    ## Method to get the matrix
    get <- function() {
        ## Return the matrix
        m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function to compute the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return the inverse if it's already calculated and cached
    m <- x$getInverse()
    if (!is.null(m)) {
        message("Retrieving cached data")
        return(m)
    }

    ## If not cached, calculate the inverse and cache it
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)

    ## Return the inverse
    m
}

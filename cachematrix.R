## Two functions, makeCacheMatrix() and cacheSolve()
## which can be used to store a matrix object and its inverse.
## The inverse is only calculated once and after that retrieved from cache.


## makeCacheMatrix(): Creates a special "matrix" object.
## Takes an invertible matrix as an argument, 
## and returns a list that allows one to get and set the matrix, 
## and set and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	# Return a list with four functions, to set and get the matrix, 
	# and set and get the inverse.
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(): Takes the special "matrix" object as an argument and sets 
## and returns the inverse of the supplied matrix.
## It either returns the inverse from cache, 
## or if there was no stored inverse, it calculates it using solve() and then stores it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()

    # if the inverse matrix is stored in cache, return that. 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # get the actual matrix from the object
    data <- x$get()

    # use solve() to get the inverse of the matrix
    inverse <- solve(data, ...)

    # use setinverse() to store the inversed matrix in cache. Then return it.
    x$setinverse(inverse)
    inverse
}
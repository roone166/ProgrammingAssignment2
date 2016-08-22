## Programming Assignment 2: Caching the Inverse of a Matrix
## Pair of functions that can cache the inverse of a matrix

## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solveMatrix) inv <<- solveMatrix
	getinverse <- function() inv
	list(set=set, get=get, 
	setinverse=setinverse, 
	getinverse=getinverse)
}


## cacheSolve computes the inverse of the "matrix" produced by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

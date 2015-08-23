## makeCacheMatrix is a special "Matrix" Object that can cache its inverse
## once it has been calculated; the inverse is obtained by calling the
## cacheSolve function

## Creates an object that holds a matrix, and is capable of caching and
## retreiving its inverse once it has been calculated

makeCacheMatrix <- function(x = matrix()) {
	# set inverse to NULL
	inverse <- NULL

	# setter function, resets the inverse
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# getter function
	get <- function() x

	# setter for inverse value
	setinverse <- function(x) inverse <<- x

	# getter for inverse value
	getinverse <- function() inverse

	# return object methods as list
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Obtains the inverse of the matrix component of the makeCacheMatrix object
## that is passed as an argument, first by checking if the inverse has
## already been cached

cacheSolve <- function(x, ...) {
	# get the currently cached inverse of x
	inverse <- x$getinverse()

	# if inverse is not NULL, then use the value obtained
	if (!is.null(inverse)) {
		message("Getting cached data")
		return(inverse)
	}

	# if inverse is NULL, calculate it and set it within the makeCacheMatrix
	# object
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)

	# return the inverse of the matrix in x
	return(inverse)
}

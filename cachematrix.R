## Programming Assignment #2 - Carol J. Gonzales
## This first function creates a square matrix which 
## returns a list containing a function to do the following
## (as in the example to cache mean):
##      1. Set the Matrix
##		2. Get the Matrix
##		3. Set the Inverse
##		4. Get the Inverse 
## Returns 1 through 4 functions as a list input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
    	set = function(y) {
        x <<- y		## using <<- operator to assign value to an object
					## different from the current environment
        inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Below Function takes the list as output from makeCacheMatrix and
## returns its inverse

cacheSolve <- function(x, ...) {
## Return a matrix which is the inverse of x using cached data if possible
	inv = x$getinv()
	if (!is.null(inv)){
	  message("Getting cached data!")
	return(inv)
	}
	matrix.data = x$get()
  inv = solve(matrix.data, ...)
	x$setinv(inv)
	return(inv)
}

## This will create a matrix that can cache its inverse using
## the makeCacheMatrix function and compute the inverse of that
## matrix using the cacheSolve

## makeCacheMatrix will cache its inverse by setting, then getting 
## the valube of the matrix, then setting and getting the value of
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
	## inv will store the cached inverse matrix
	inv <- NULL

	## setting the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## getting the matrix
	get <- function() x

	## setting the inverse
	setinverse <- function(inverse) inv <<- inverse

	## getting the inverse
	getinverse <- function() inv

	## return the matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix.  If it's already calculated
## it returns that with a note to that effect

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()

	## if the inverse is already calculated, get it
	if (!is.null(inv)) {
		message("The inverse has been calculated.")
		return(inv)
	}

	## otherwise, calculate the inverse
	data <- x$get()
	inv <- solve(data, ...)

	## cache the inverse
	x$setinv(inv)

	## return the inverse)
	inv
}

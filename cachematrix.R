## The function makeCacheMatrix will generate a matrix object that can cache its inverse
## The function cacheSolve will compute and return the inverse of the matrix created by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- inv
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

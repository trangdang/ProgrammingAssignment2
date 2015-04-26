## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y) {
		x <<- y
		minv <<- NULL
	}
	get <- function() x
	setinverse <- function(val_inv) minv <<- val_inv
	getinverse <- function() minv
	list(set = set, get = get, 
		setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	minv <- x$getinverse()
	if(!is.null(minv)) {
		message("getting cached data")
		return(minv)
	}
	
	data <- x$get()
	minv <- solve(data, ...)
	x$setinverse(minv)
	return(minv)
}

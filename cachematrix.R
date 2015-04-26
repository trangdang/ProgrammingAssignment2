## Creates a "matrix" object x that can cache its inverse.
## x is a list of the functions:
## get, set, getinverse, setinverse
##
## invariant: getinverse() should only ever return NULL 
## or the correct inverse of x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	get <- function() x
	set <- function(mat_val) {
		x <<- mat_val
		inv <<- NULL # if matrix is being changed, reset the inverse
	}
	
	getinverse <- function() inv
	setinverse <- function(inv_val) inv <<- inv_val
	
	list(set = set, get = get, 
		setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
		
	## try to retrieve x's inverse
	inv <- x$getinverse()

	if(!is.null(inv)) {
		## cached inverse exists so inform user that cachce is
		## being made use of, and return inverse
		
		message("getting cached data")
		return(inv)
		
	} else {
		## inverse isn't cached (is null). Solve for the inverse,
		## then store in cache and return result
		
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv) # store in cacche
		return(inv)
		
	}
	
}

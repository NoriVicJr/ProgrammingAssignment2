# The function makeCacheMatrix creates a special kind matrix object 
# that is able to cache its inverse.


makeCacheMatrix <- function(x = matrix()){
	inv_mat <- NULL
	set <- function(y){
		x <<- y inv_mat <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv_mat <<- inverse
	getinverse <- function() inv_mat
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function calculates the inverse of the matrix x created with
# the makeCacheMatrix function above.
# It checks if the inverse has already been calculated. If
# not, it computes, caches, and returns it.


cacheSolve <- function(x, ...){
## Check for cached data
	inverse <- x$getinverse()
	if (!is.null(inverse)){
## If there is already cached data: return it.
		return(inverse)
	}
## If there is already cached data: Solve for the inverse and cache it.
	dt <- x$get()
	inverse <- solve(dt, ...)
	x$setinverse(inverse)
	## Return the inverse.
	inverse
}

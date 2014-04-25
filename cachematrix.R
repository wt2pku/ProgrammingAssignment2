## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse;
	getinv <- function() inv
	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()							#query the matrix x's cache
		if(!is.null(inv)) {							#if there is a cache
			message("getting the cached data")
			return(inv)								#just return the cache, no computation needed
		}
		
		mat <- x$get()								#if there's no cache
		inv <- solve(mat,...)							#compute the inverse here
		x$setinv(inv)								#save the result back to x's cache
		inv											#return the result
}

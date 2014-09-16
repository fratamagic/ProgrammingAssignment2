# makeCacheMatrix does the following:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
			}
		get <- function () x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)

}


# The cacheSolve function computes the inverse of a matrix 
# returned by makeCacheMatrix. If the inverse has already 
# been calculated, cacheSolve retrieves the result from cache. 
# If there is no stored value for the inverse, cacheSolve computes 
# the inverse of the matrix and stores the value in setInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if(!is.null(inv)) {
				message("getting cached data")
				return(inv)
			}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
		inv
}

# Testing: 
# x <- rbind(c(1, 2), c(3, 4))
# simple_matrix <- makeCacheMatrix(x)
# cacheSolve(simple_matrix)

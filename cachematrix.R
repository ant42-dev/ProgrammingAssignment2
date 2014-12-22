## These functions help create a specialized version of a Matrix
## that only needs to calculate its inverse a single time.
## Any calls for its inverse later on will return the cached
## version of the previously calculated inverse matrix.
## 

## makeCacheMatrix() creates a simple Matrix that will cache its 
## inverse when it calculates it for the first time.

makeCacheMatrix <- function(x = matrix()) {
	## store our matrix in 'm'
	m <- NULL

	## define set(), get(), and list() functions
        set <- function(y) {
		x <<- y
		m <<-NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m

	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}



## cacheSolve uses solve() to calculate the inverse of the matrix 
## passed in (if necessary).   If previously calculated then it
## just returns the previously calculated "cached" value.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached inverse matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
	m
}

## makeCacheMatrix:This function creats a special"matrix"object that can cache its inverse.
## casheSolve:This function computes the inverse of the special "matrix"returned by makeCacheMatrix above.

## 1.set the value of the matrix
## 2.get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
	x <<- y
	m <<- NULL
	}

	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = setmatrix)
}


## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}


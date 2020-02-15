## Put comments here that give an overall description of what your
## functions do

## Function "makeCacheMatrix" is for creating a list of functions that can set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix, and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	setMat <- function(y) { 
		x <<- y
		inv <<- NULL
	}
        getMat <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
        list(setMat=setMat, getMat=getMat, setInverse=setInverse, getInverse=getInverse)
}


## Function "cacheSolve" is to calculate and save the inverse of the matrix if there is no cached inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()

        ## Check whether there is a cached inverse

	if (!is.null(inv)){
		message("getting the cached data")
		return(inv)
	}

       ## If not, calculate the inverse and save it
	mat <- x$getMat()
	inv <- solve(mat, ...)
	x$setInverse(inv)	
	inv
        
       ## Return a matrix that is the inverse of 'x'
}

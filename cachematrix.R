## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.


## This function create a list containing a function to:
## 1) "set": set the value of the matrix
## 2) "get": get the value of the matrix
## 3) "setinv": set the inverse of the matrix
## 4) "getinv": get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	I<-NULL
 	set <- function (y){
 		x <<- y
 		I <<- NULL
 	}
	get <- function() x
	setinv <- function(inv) I <<- inv
	getinv <- function() I

	list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed) then the cachesolve
## retrieves the inverse from the cache, otherwise it computes the inverse, stores the result and returns it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	I <- x$getinv()
	if(!is.null(I)) {
		message("getting cached data")
		return(I)
	}
	d<-x$get()
	I<-solve(d, ...)
	x$setinv(I)
	I
}

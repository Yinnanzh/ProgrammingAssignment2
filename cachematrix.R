## Matrix inversion with caching the inverse of a matrix

## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL  ## ix is the matrix that cache the inverse of x
	set <- function(y){
		x <<- y
		ix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) ix <<- inverse
	getinverse <- function() ix
	list(set=set,get=get, 
	     setinverse=setinverse, 
	     getinverse=getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if(!is.null(ix)){
        	message("getting cached data")
        	return(ix)
        }
        data <- x$get()
        ix <- solve(data,...)
        x$setinverse(ix)
        ix ## Return a matrix that is the inverse of 'x'
}

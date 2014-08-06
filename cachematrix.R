## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Matrix object to cache inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse property
    i <- NULL

    ## set the Matrix
    set <- function( matrix ) {
        m <<- matrix
	i <<- NULL
    }
   
    ## Get the matrix
    get <- function() {
       m
    }

    ## Set inverse of the matrix
    setInverse <- function(inverse) {
       i <<- inverse
    }


    ## Get Inverse of Matrix
    getInverse <- function() {
      i
    }
    
    ## Return method lists
    list(set = set, get = get, 
         setInverse = setInverse, 
	 getInverse = getInverse
        )

}


## Write a short comment describing this function
## Calc inverse of the spl matrix returned  by above makecache matrix.
## cacheSolve will get the inverse from the cache depending upon
## whether the inv is already computed or not......

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()

	## return just the inv if it is already set
	if( !is.null(m) ) {
     	   message("pulling cached data")
	   return(m)
	 }

	 ## pull matrix from object
	 data <- x$get()

	 ## Calc inverse
	 m <- solve(data) %*% data

	 ## set inv to the object
	 x$setInverse(m)

	 ## return matrix
	 m
}

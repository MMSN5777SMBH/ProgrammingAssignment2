###	 Example: Caching the Inverse of a Vector
##
##	In the following code, the `<<-` operator is used.
##	It assigns a value to an object in an environment that is different from the
##	current environment. 

##	Below are two functions that are used to create a
##	special object that stores a numeric matrix and caches its inverse.
##
##	The first function, `makeCacheMatrix` creates a special "matrix" object 
##	that can cache its inverse.

##	`makeCacheMatrix` creates a special "matrix", which is
##	really a list containing a function to
##
##	1.  set the value of the matrix
##	2.  get the value of the matrix
##	3.  set the value of the inverse
##	4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
		
		invr <- NULL
		
		set <- function(y) {
		
			x <<- y
			invr <<- NULL
    	
    	}
    	
		get <- function() x
		
		setinverse <- function(inverse) invr <<- inverse
		getinverse <- function() invr


		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


##	cacheSolve: This function computes the inverse of the special "matrix" 
##	returned by makeCacheMatrix above. 

##	If the inverse has already been calculated (and the matrix has not changed), 
##	then the cachesolve retrieves the inverse from the cache.

##	Computing the inverse of a square matrix is done with the solve function in R. 
##	For example, if X is a square invertible matrix, then solve(X) returns its inverse.




		cacheSolve <- function(x, ...) {

				invr <- x$getinverse()

				if(!is.null(invr)) {
				
        			message("getting cached data.")
					return(invr)
    			}

				data <- x$get()

				invr <- solve(data)
				
				x$setinverse(invr)
				
				invr

		}

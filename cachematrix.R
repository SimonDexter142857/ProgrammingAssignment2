#################################################
# Author:   Simon Dexter, simondex@yahoo.com
# Date:     08/17/2014
#
# Description: 
#           Cache matrix programming assignment
#################################################


#----------------------
# Returns a special matrix object which maintains 
# its inverse in cache
#----------------------

makeCacheMatrix <- function(x = matrix()) {
	
	# storage for cache
	inv = NULL;
	
	# setting matrix data and initializing
	# inverse matrix to null
	set = function(y) {
		x <<- y; 
		inv <<- NULL;
	}

	
	# returning matrix data
	get = function () x;
	
	# setting the inverse if not computed yet
	setInverse = function(inverse) inv <<- inverse;
	

	# getting the inverse 
	getInverse = function() inv; 
	
	# creating and returning the 
	# resultant object
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse);
	
}



#----------------------
# Retrieves inverse of matrix contained 
# in an object returned by makeCacheMatrix
# function
#----------------------

cacheSolve <- function(x, ...) {
   inv = x$getInverse(); 
      if (!is.null (inv)) {
           message ("getting cached data"); 
           return(inv); 
      }
		
		data = x$get();
		i = solve(data); 
		x$setInverse (i);
		
		i;
        ## Return a matrix that is the inverse of 'x'
}

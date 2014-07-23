## Author Ramesh M
## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Create speacial matrix object - inverse objec
        # initilize inverse object
        matrix_inverse <- NULL
          
        # set method,  set method object  to matrix object 'x'
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        
        # Get cache matrix  object as inverse
        get <- function() x
        
        # Set inverse matrix object
        setinverse <- function(i)  matrix_inverse <<- i
        
        # Get inverse matrix 
        getinverse <- function() matrix_inverse
        
        # listing Set and Get methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        # get matrix value
        m <- x$getinverse()
       
        # Check if its not null
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # read  matrix value into data
        data <- x$get()
        
          
        # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        # use slove function to reverse the values
        m <- solve(data)
        
        # set reversed value for retrun 
        x$setinverse(m)
        # Return inversed value
        m
}

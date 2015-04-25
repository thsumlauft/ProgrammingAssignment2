## makeCacheMatrix is a function similar to a class which creates a list of functions.
## makeCacheMatrix stores a matrix as well as a cached inversed matrix.
## The function list contains the following functions:
## 
## 1) set: sets the value of a matrix
## 2) get: gets the value of a matrix
## 3) setinv: gets the value of the inversed matrix
## 4) getinv: sets the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        ## sets m to NULL
        set <- function(y) {
                x <<- y
                ## resets matrix
                m <<- NULL
                ## sets m to NULL when matrix is changed
        }
        
        
        get <- function() x
        ## return the input matrix   
        setinv <- function(solve) m <<- solve
        ## sets the inversed matrix
        getinv <- function() m    						
        ## returns the inversed matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)		
        ## returns a list with above listed functions
}


## "cacheSolve" function computes the inverse of matrix x returned by "makeCacheMatrix" (see above). 
## If the inverse has already been calculated and the matrix is unchanged, 
## "cacheSolve" retrieves the inverse from the cache.

cachesolve <- function(x, ...) {
        
        m <- x$getinv()
        ## gets the inversed matrix from x
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
                ## if m is calculated, the message "getting cached data" is being displayed and m returned
        }
        
        
        data <- x$get()
        ## if m is not calculated, x$get retrieves the matrix
        m <- solve(data, ...)
        ## which is then being inverted by solve()
        x$setinv(m)
        ## and subsequently set to the object
        return(m)								
        ## return(m) then returns the result
}	

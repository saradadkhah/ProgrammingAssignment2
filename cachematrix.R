## Matrix inversion is costly computationally so we are going to cache it


## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.

## Basically, I copied over the makeVector example from class instructions
## and replaced 'mean' with 'solve'

makeCacheMatrix <- function(x = matrix()) {
                        ## initially set to null but will change if 
                        ## user sets value
                        m <- NULL  
        ## first we set the matrix, but not the inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }        
        ## get function gets the matrix itself but not the inverse
        get <- function() x
        # here we set the inverse
        setsolve <- function(solve) m <<- solve
        # then we get the inverse
        getsolve <- function() m
        # put it in a list format
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Get the current state of the inverse
        
                m <- x$getsolve()
                
                ## see whether it has been computed yet
                if(!is.null(m)) {
                        ##if so, display a message and return cached values
                        message("getting cached data")
                        return(m)
                }
                
                ## if it hasnt been computed already, get the matrix
                data <- x$get()
                m <- solve(data, ...)
                # Cache this result 
                x$setsolve(m)
                #return the new result
                m
        }
        


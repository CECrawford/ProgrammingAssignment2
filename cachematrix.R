
## Example usage of the functions as follows:
####################################################################################
##  m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##  a <- makeCacheMatrix(m1)
##  cacheSolve(a)
##  running cachesolve(a) a subsequent time should return with a message "getting
##  cached data"
#####################################################################################

## This "makeCacheMatrix" function creates a special "matrix" 
## object that can cache its inverse
## The function assumes that the matrix supplied is a square invertible matrix
makeCacheMatrix <- function(inputMatrix = matrix()) {
        ##initialise object which will hold the inverse
        inverseMatrix <- NULL
        
        ##set which assigns input argument to inputMatrix object in parent environment
        ##clears any value of inverse that was previously cached
        set <- function(y) {
                inputMatrix <<- y
                inverseMatrix <<- NULL
        }
        
        ##get retrieves value
        get <- function() inputMatrix
        
        ##inverts matrix and uses <<- to assign to value of inverseMatrix in parent environemnt
        setInverse <- function(solve) inverseMatrix <<- solve
        
        #Retrieves value of the inverse
        getInverse <- function() inverseMatrix
        
        #creates named values so that can use $ form when calling from downstream code
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


#This "cacheSolve" function computes the inverse of the
##special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then this returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()
        
        ## if null get from cache
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        
        ##cache was null, so need to solve for inverted matrix 
        ##calls get, then calls solve to solve for inverse, then calls set to set on
        ##the input object and returns value of inverse to the parent environment
        data <- x$get()
        invM <- solve(data, ...)
        ##line below sets inverse on the input object
        x$setInverse(invM)
        invM
}


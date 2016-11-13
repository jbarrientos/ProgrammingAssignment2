## Function to cache matrix inversion given an invertible matrix 
## 

## The function makeCacheMatrix() expects an invertible matrix as argument (x)
## the matrix inversion as the matrix are cached thru the methods setMatrixInversion()
## and get() respectively.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        get <- function() x 
        
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        setMatrixInversion <- function(mi){
                m <<- mi
        }
        getMatrixInversion <- function(){
                m
        }
        
        list(set = set, get = get, setMatrixInversion = setMatrixInversion, getMatrixInversion = getMatrixInversion)
        
}


## caheSolve(): function that resolve if there's an inversion matrix cached by the 
## makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getMatrixInversion()
        
        ## If the inversion matrix is cached then it is returned, otherwise
        ## a matrix is returned by the makeCacheMatrix() function and then the 
        ## matrix (data) it is inversed, using the solve() function with the invertible 
        ## matrix (data) as unique parameter. 
        if(!is.null(m)){
                ## Cached inverted matrix is returned.
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data)
        
        x$setMatrixInversion(m)
        
        ## Inverted matrix is returned if it's not cached
        m
}

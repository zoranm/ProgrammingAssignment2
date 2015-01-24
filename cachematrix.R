## These two functions are used to perform inverse matrix computaton 
## while making use of caching to improve efficiency of potentially time-consuming computations. 
## The first function defines a list of anonymous functions to manipulate inverse matrices and 
## the second one  performs inverse matrix computation using R solve function
## Note that these examples closely follow the solution pattern provided in the assignment example

## This function defines a list of functions in support of storing and managing cache
## area for storing an inverse matrix data. The function makes use of the scoping rules 
## of the R language and how they can be used to preserve state inside of an R object

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(solve) im <<- solve
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates inverse matrix of a matrix. It uses solve R function to
## compute inverse of the matrix.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'. 
    ## The function first checks whether the inverse matrix has already been computed. 
    ## If so, it gets the inverse funtion from the cache and skips the computation. 
    ## Otherwise, it computes the inverse matrix and sets its value in the cache via 
    ## the setinv function.
    
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    datamatrix <- x$get()
    im <- solve(datamatrix, ...)
    x$setinv(im)
    im  
      
}

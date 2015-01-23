## This two functions are used to perform inverse matrix computaton 
## while making use of caching to improve efficiency of potentially time-consuming computations. 
## The first function defines a list of functions to manipulate inverse matrices, while the second
## function performs inverse matrix computation using solve function
## Note that these examples closely follow the solution pattern provided in the assignment example

## This function defines a list of functions in support of storing and managing cache
## area storing an inverse matrix data. The function makes use of the scoping rules 
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
        ## Return a matrix that is the inverse of 'x'
    
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

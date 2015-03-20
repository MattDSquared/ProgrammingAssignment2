## cachematrix creates two functions for creating and handling a special 
## "matrix" object which can cache the results of the matrix's inverse. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## source: https://class.coursera.org/rprog-012/human_grading/view/courses/
## 973493/assessments/3/submissions
##
## makeCacheMatrix() returns a "matrix" object with the below function list.
##      These functions return empty values.
## makeCacheMatrix(x) returns a special "matrix" object which contains a list
##      of data manipulation functions (see below). These functions are used by
##      cacheSolve to access and modify the matrix data inverse.
## Returns list of the following functions: 
##      $set(y): sets the internal data variable (note the internal data can also
##              be set by calling makeCacheMatrix(x))
##      $get(): returns the internal matrix data
##      $setSolve(solution): sets the cache for the solution (note: no computation 
##              occurs inside of makeCacheMatrix)
##      $getSolve(): returns the cached solution value (returns NULL if 
##              setSolve has not been called for this object instance yet)
makeCacheMatrix <- function(x = matrix()) {
    ## initialize local (internal) variables
    s <- NULL
    
    ## build functions for setting and retreiving data
    set <- function(y) {
        ## assign value to variable x and s in enclosing environment defined 
        ## here. Lexical scoping rules mean calling set(y) from anywhere else 
        ## modifies the values in variables assigned within makeCacheMatrix
        ## for those familiar with OOP, this is sort of an implied object 
        ## dereferencing. 
        x <<- y
        # reset the cached solution since the data changed
        s <<- NULL
    }
    get <- function() x
    
    ## build functions for getting and setting the cache
    setSolve <- function(solution) s <<- solution
    getSolve <- function() s
    
    ## return function list
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve computes the inverse the above special "matrix" object only on
## its first execution with each "matrix". Returns this matrix inverse. After  
## the inverse has been solved, cacheSolve returns a cached copy of the inverse. 
## source: https://class.coursera.org/rprog-012/human_grading/view/courses/
## 973493/assessments/3/submissions
##
## cacheSolve(x), where x is the returned function list from makeCacheMatrix.
## Returns: inverse of "matrix" x.
cacheSolve <- function(x, ...) {
    ## retrieve current value of cached solution
    s <- x$getSolve()
    
    ## if the cached solution has already been calculated, return it
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## retrieve the data stored inside special "matrix" object and solve for
    ## the inverse (assumes matrix is invertible)
    data <- x$get()
    s <- solve(data, ...)
    
    ## cache this solution in the special "matrix" object
    x$setSolve(s)
    
    ## Return a matrix that is the inverse of 'x'
    s
}

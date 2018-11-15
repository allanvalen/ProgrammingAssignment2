## makeCacheMatrix function takes a matrix as object argument to store or lodge 
## such within the environment created by this function. At the same time, this
## function also stores another four functions: (1) change the stored matrix [set],
## (2) call the stored matrix [get], (3) create object where we can store the inverse
## matrix [setsolve], and (4) call the stored inverse matrix [getsolve]. These four
## functions within the makeCacheMatrix function can be called by extracting the names
## containing the four functions defined by the list within the makeCacheMatrix function.
## cacheSolve function caches and returns the inverse of the matrix stored and 
## calculated in makeCacheMatrix function.

## makeCacheMatrix function can cache an inputted matrix and its computed inverse within
## its own environment, while having the ability to extract cached matrix or inverse matrix 
## and to amend such inputted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                               ## Initialize x as blank matrix and m as NULL, then
    set <- function(y) {                    ## create set function to be able to manually
        x <<- y                             ## change the value of x and m when desired.
        m <<- NULL                          ## Using the global assignment <<-, objects x
    }                                       ## and m in the parent environment are updated.
    get <- function() x                     ## Create get function to get the value of x.
    setsolve <- function(solve) m <<- solve ## Create setsolve function to create an object
                                            ## where we cache the inverted matrix using the
                                            ## cacheSolve function (discussed below).
    getsolve <- function() m                ## Create getsolve function to get the inverted matrix.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)               ## Create list where each element is the four functions
}                                           ## defined within the makeCacheMatrix environment
                                            ## as a way to be able to call the functions easily.


## cacheSolve takes the makeCacheMatrix function as an argument with all the other 
## properties and functions it carries to retrieve the inverse of the matrix stored in 
## makeCacheMatrix's environment, and then return the cached inverse.

cacheSolve <- function(x, ...) {            ## Define cacheSolve function with MakeCacheMatrix as its argument.
    m <- x$getsolve()                       ## Retrieve to see if there is any cached inverse matrix
    if(!is.null(m)) {                       ## Logical test if there is matrix in the object.
        message("getting cached data")      ## If there's inverse matrix, then print and
        return(m)                           ## return the inverse matrix in the object.
    } else {                                ## But if m did not retrieve any inverse matrix, then
        data <- x$get()                     ## retrieve the original matrix to recompute the inverse matrix.
        m <- solve(data, ...)               ## Re-compute the inverse matrix, then
        x$setsolve(m)                       ## cache the re-computed inverse matrix to makeCacheMatrix's environment.
        m                                   ## If logical test above is true, then this is redundat and
    }                                       ## will not return any value.
}
## else statement above is important, otherwise, the program will at times unnecessarily
## re-compute the inverse matrix, which takes up memory and we don't want it to happen.
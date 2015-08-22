##=================================================================================
## Contains a pair of functions that cache the inverse of a matrix.
## Example:
##   m <- matrix(data = c(3, -7, 2, -4, 0, -1, 1, -2, 4),nrow=3,ncol=3,byrow=TRUE)
##   myMat <- makeCacheMatrix()
##   myMat$set(m)       ## input the matrix
##   myMat$get()        ## display the matrix m
##   cacheSolve(myMat)  ## display the inverse of matrix m
##
## Author: Ashan Kalpage
## Date  : 22/08/2015
##=================================================================================


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL  ## matInv = inverse of a matrix
    
    ## (1) set - user to input a square matrix
    set <- function(y)
    {
        x      <<- y
        matInv <<- NULL
    }
    
    ## (2) get - return the matrix specified by set
    get <- function() x
    
    ## (3) setMatInv - user to input Inverted matrix
    setMatInv <- function(solve) matInv <<- solve
    
    ## (4) getMatInv - return the inverse of a matrix
    getMatInv <- function() matInv
    
    ## Return the following list
    list(set       = set,
         get       = get,
         setMatInv = setMatInv,
         getMatInv = getMatInv)

}


## cacheSolve: computes the inverse of the special "matrix" returned 
##             by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatInv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m    <- solve(data, ...)
    x$setMatInv(m)
    m
}

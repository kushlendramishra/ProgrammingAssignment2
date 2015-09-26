## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function craetes a special matrix object that can cache its inverse.
#It takes a matrix as input and returns a list of functions that enable the user to manipulate the object.
#The set function enables the user to 
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(matrixInv)  mInv <- matrixInv
        getinv <- function() mInv
        
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv )
}


## Write a short comment describing this function
# this function returns the cached inverse of the matrix of 'cachedMatrix' type
# if it has been calculated previously, or else calulates the inverse, caches it 
#and then returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinv()
        if (!is.null(matinv)) {
                message("getting cached inverse")
                return(matinv)
        }
        thismatrix <- x$get()
        thisMatrixRows <- nrow(thismatrix)
        thisMatrixCols <- ncol(thismatrix)
        if (thisMatrixRows != thisMatrixCols) {
                stop("Cannot calculate the inverse of non-square matrix")
        }
        matinv <- solve(thismatrix)
        x$setinv(matinv)
        matinv
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object 
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        mBase <- NULL
        set <- function(y){
                x <<- y
        }
        get <- function() x
        setBmatrix <- function(bmatrix) mBase <<- bmatrix
        getBmatrix <- function() mBase
        setImatrix <- function(imatrix) mInv <<- imatrix
        getImatrix <- function() mInv
        list(set = set, get = get, 
             setImatrix = setImatrix,
             getImatrix = getImatrix,
             setBmatrix = setBmatrix,
             getBmatrix = getBmatrix)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix created by makeCacheMatrix. If the 
## inverse is already calculated then the inverse from cache is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getImatrix()
        nmatrix <- x$get()
        if(!is.null(m)){
                if (isTRUE(all.equal(x$getBmatrix(),nmatrix))){
                        message("getting cached data")
                        return(m)        
                }
        }
        x$setBmatrix(nmatrix)
        m <- solve(nmatrix)
        x$setImatrix(m)
        m
}



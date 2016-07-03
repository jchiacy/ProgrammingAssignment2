## This is a pair of function that cache the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" pbject that can cache its inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## This is the method to set the matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## This is the method to get the matrix
    get <- function()x
    
    ## This sets the inverse of the matrix
    setinverse <- function(solve)m <<- solve
    
    ## This gets the inverse of the matrix
    getinverse <- function()m
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of makeCacheMatrix. 
## If the inverse has been computed and the matrix has not been changed, 
## then the cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
        ## If the inverse has been set, retrieve the inverse from cache
        if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
        
    ## Else if, compute the inverse using Solve()
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

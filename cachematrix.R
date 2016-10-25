## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Assuming matrix is not invertible
    dimensions <- dim(x)
    if((dimensions[1] != dimensions[2]) || 
       (dimensions[1] == 1 || dimensions[2] ==1)){
        return ("Matrix not invertible")
    }
    
    inv <- NULL
    
    get <- function() x
    set <- function(matrix){
        x <<- matrix
        inv <<- NULL
    }
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(get = get,
         set = set,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Getting cached inverse and checking if is NULL or no
    ## If is NULL then calculate inverse matrix with solve
    ## and put newly calculated inverse matrix in cache
    inverse <- x$getinverse()
    if(is.null(inverse)){
        message("Callculating cached inverse matrix")
        inverse <- solve(x$get())
        x$setinverse(inverse)
        return (inverse)
    }
    
    ## If cached inverse matrix is allready calculated
    ## Check if calculated inverse matrix from original is the same as cached
    ## If it's same then return cached, else Return tha Cached and Inverse are not the same
    
    data <- x$get()
    inverse_solve <- solve(data)
    if(identical(inverse, inverse_solve)){
        x$setinverse(inverse)
        message("Cached inverse matrix is the same as newly calculated")
        return (inverse)
    }
    
    return ("Inverse and Cached Inverse are not the same")
}

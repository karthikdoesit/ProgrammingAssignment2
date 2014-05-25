## R functions to cache potentially time-consuming computation of matrix inverse
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve retrieves the inverse from the cache.

## This function creates a special matrix which holds a matrix and its inverse in cache, 
## provides access to matrix and inverse through the get and set methods of the special matrix object
makeCacheMatrix <- function(x = matrix()) {
    
    ## setting null value to the inverse matrix
    inverseMatrix <- NULL        
    
    set <- function(y){
        ## Verify if the new matrix is the same one as existing matrix
        if(!(is.matrix(x) && is.matrix(y) && dim(x)==dim(y) && all(x ==y)))
        {            
            x <<- y
            inverseMatrix <<- NULL
        }        
    }
    
    get <- function() x
    
    setInverse <- function(solve)  inverseMatrix <<- solve
    
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
    
}

## Computes the inverse of matrix within x if doesn't exist, 
## if it already exist returns the inverse from the cache through getInverse function of x
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverseMatrix)){            
        message("getting cached inverse")
        return(inverseMatrix)        
    }    
    mat <- x$get()
    inverseMatrix <- solve(mat, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}

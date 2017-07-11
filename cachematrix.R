## These are functions to initiate and calculate inverse matrices with caching feature.
## First, a special list-object is created from a given matrix
## and second, the inverse matrix is calculated and cached.

## The first function create a list of the basic functions and the matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x)) { 
                message("not a matrix")
                return(head(x))  
        }    
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL  
        }    
        get <- function() x  
        setInv <- function(solve) inv <<- solve  
        getInv <- function() inv  
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function calculates the inverse and put the result in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}

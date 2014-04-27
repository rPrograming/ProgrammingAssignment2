## makeCacheMatrix: stores the matrix x passed as an argument
## in another environment and returns a list of functions
## 1) set: stores the passed matrix and resets the inverse
## 2) get: gets the passed matrix
## 3) setinverse: sets the inverse matrix in another environment
## 4) getinverse: gets the inverse of the matrix either by accessing the 
##                cached data or recalculating it. 

## return: List  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv  <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Returns the inverse of a matrix, either returning the cached 
##             instance or solving it anew. 

## parameters: x - A list created by the function makeCacheMatrix 

## return: a Matrix inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}






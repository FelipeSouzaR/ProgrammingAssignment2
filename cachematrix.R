## Based on the example for the assignment this first method 'makeCacheMatrix' 
## creates the new matrix object that can cache its inversed

## 1. Create the inverted matrix variable empty
## 2. Defines the 4 basic crud methods, get and set, normal and inversed
## 3. Defines the list of functions inside the Scope of this object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse_n) inverse <<- inverse_n
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## 1. Try to get the Inverse Matrix from the cache to avoid a new calculation
## 2. If it exists, returns, if not, continues to the calculation
## 3. Brings the matrix to its local environ
## 4. Uses the solve method without a 'b' arguments so it returns the inverse of 'a'
## 5. stores the inversed matrix to the cache and returns it

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

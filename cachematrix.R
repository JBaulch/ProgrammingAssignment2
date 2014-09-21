## Within this file there are two functions that aid in computing the inverse
## of a matrix. We have utilized the caching feature of R to aid in
## computational efficiency

## The makeCacheMatrix function creates a list of getters and setters of a 
## matrix. The getters and setters exist for both the matrix itself as well 
## as its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y){
                x <<- y
                matrix <<- NULL 
        }
        get <- function() x
        setInverse <- function(inverse) matrix <<- inverse
        getInverse <- function() matrix
        list (set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}


## This function will return the inverse of a special matrix that was
## created through the makeCacheMatrix function. It will either return the
## cached version if the matrix inverse has previously been computed or
## calculate the inverse, caching it, and then returning the value.

cacheSolve <- function(x, ...) {
       
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

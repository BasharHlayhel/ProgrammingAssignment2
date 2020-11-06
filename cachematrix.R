#it decreases computation time by caching data
#there are two functions and each have a specific role


#this function cache the data
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse_2 <- function() {inv}
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    

}

#this function checks whether there is a cached data
#and calculate the inverse if no cache data is there

cacheSolve <- function(x, ...) {
       
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}


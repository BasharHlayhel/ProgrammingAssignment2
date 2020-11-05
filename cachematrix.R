
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


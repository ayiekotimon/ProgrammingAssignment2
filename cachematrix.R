## makeCacheMatrix is a function that takes a matrix as its argument and then
## defines other functions which do the following:
##    1. get() - takes no argument but returns the matrix given into the makeCacheMatrix
##    2. set() - set a new matrix instead of the one already give into the makeCacheMatrix
##       if there is need and the value for the inverse to be null.
##    3. setinverse() - sets the inverse matrix to the matrix given as its argument
##    4. getinverse() - returns the inverse matrix when called.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list( get = get,
          set = set,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve function takes as its argument an object containing the makeCacheMatrix function
## It first gets the inverse matrix and check whether it has been evaluated and then returns it 
## else it goes ahead and calculates it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

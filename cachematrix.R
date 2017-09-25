## cachematrix.R is a source code that cacheing the inverse of a matrix rather
## than compute it again. By doing so, it saves time and ram on computing matrix
## inverse if it is already calculated.

## makeCacheMatrix function can create a special "matirx" object which can cache 
## its inverse. The four functions inside of makeCacheMatrix serve different 
## purposes: set value of the matrix, get value of the matrix, set value of the 
## matrix inverse and get value of the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set <-function(y){
        x<<- y
        m<<- NULL
    }
    get <-function() x
    setinverse <-function(solve) m<<-solve
    getinverse <- function() m
    list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve function computes the inverse of the special "matix" object
## created with the makeCacheMatrix function, but if the inverse has been 
## calculated, it gets the cache and skips the calculation process. But if the
## inverse has not been calculated, it will calculate the inverse of the matrix
## and return it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message('getting cached data')
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## MakeCacheMatrix() returns list of functions to the parent environment. These 
## functions allow to set and get (retrieve) the values of two objects - one is
## a matrix (object x) and another one could be used to store the inverse of a matrix (object s). 
## This is used by cacheSolve() function which checks if any value is stored for
## the inverse of a matrix and returns it. Alternatively, if none is stored, it computes the 
## inverse and saves it in the object s, so that later this computation doesn't have to be repeated,
## so it can be used to cache the inverse of a matrix.

## makeCacheMatrix() has one argument which is an empty matrix by default.
## Inside it's environment four functions are defined, and two objects 
## are stored: x, which is an argument of the function, and s, which is set to be NULL, 
## but coud be changed by calling setsolve() function. 
## Similarly, calling set() function has the power to change value of x in the environment of 
## makeCacheMatrix() to the value passed to set() it as an argument. 
## get() and getsolve() allow to acess the objects x and s.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) {
        s <<- solve
    }
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## When MakeCacheMatrix() is called and the result is assigned to a new object, this resulting object 
## will have a copy of MakeCacheMatrix() environment, as well as values set for x and s objects.

## cacheSolve() takes such resulting object as an argument, so that it can access and modify 
## the values stored for x and s. 
## If the s was set/computed before, so that it is not NULL it will be printed after 
## "getting cached data". If not, it will be computed through solve(), returned and the result will 
## be stored in the object s (in the environment created earlier by calling MakeCacheMatrix()).

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s  ## Return a matrix that is the inverse of 'x'
}

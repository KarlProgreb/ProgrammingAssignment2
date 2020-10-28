## The 2 following functions compute the inverse of a matrix and save it to cache
## so that during a repeated attempt, the previously saved value is returned
## instead of actually calculating it again.

## makeCachematrix function generates a certain matrix object, which in essence 
## is a list of containing a function to set and get the value of the matrix,
## and set and get the value of the inverse.

makeCachematrix <- function(x = matrix()) {

  ## creates a matrix object "x"
  ## defines the cache c
  c <- NULL
  set <- function(y) {
    x <<- y ## assigns the input matrix y to the variable x in the parent
    ## environment
    c <<- NULL ## reinitialize c in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) c <<- inverse ## set the cache c equal to the
  ## inverse of the matrix x
  getinverse <- function() c ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes the inverse of the matrix created as a result
## of the function above. Only this time, it first checks to see if the inverse
## has already been calculated beforehand. If it has, then it "get"s the inverse
## from the cache and skips the calculation. If it hasn't, it calculates the 
## matrix inverse and sets it's value in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  c <- x$getinverse()
  if(!is.null(c)) {
    message("returning cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinverse(c)
  c
}
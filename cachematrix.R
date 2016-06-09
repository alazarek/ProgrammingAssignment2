# clears the console
clear <- function() {
  cat("\014")
}
## This function creates a "special" matrix
makeCacheMatrix <- function (x = numeric()) {
  m <- NULL # setting me to null
  set <- function(y) { # this line sets set to a funtion
    x <<- y ## setting x to y and creating a "global variable
    m <<- NULL ## setting m to Null again
  } # ending the funciton 
  get <- function() x # setting get to a function returning 1 value x
  setinverse <- function(solve) m <<- solve ## setting setinverse to a function which applies the solve function to m
  getinverse <- function() m ## setting getinverse to a function which returns the value of m
  list(set = set, get = get, ## setting the list of options to be used by an outside call
        setinverse = setinverse, ## setting setinverse to setinverse
        getinverse = getinverse)# setting getinverse to getinverse
}

# this function can only be used after the call of the makeCacheMatrix function is used.
# this function will return the inverse of a "special" matrix
cacheSolve <- function (x, ...) {
  m <- x$getinverse() #setting m = x
  if(!is.null(m)) { # was x not null?
    message("getting cached data") # echo to the console
    return(m) # exit and return m
  }# end if
  data <- x$get() # setting data = x
  m <- solve(data, ...) # getting the inverse of m
  x$setinverse(m) # setting the "global" variable so that it stores the inverse
  m # exit and return m
}
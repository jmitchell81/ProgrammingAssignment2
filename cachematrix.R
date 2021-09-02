## Put comments here that give an overall description of what your
## functions do

## Creates a CacheMatrix object that stores the functions used to get and
## set the target matrix and its inverse, as well as variable x that stores
## the passed matrix and I that stores this matrix's inverse

## <<- specifically assigns a value in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    # changes x to the new value y in makeCacheMatrix env
    x <<- y
    
    # ensures I is now NULL so that a leftover solve() for a previous
    # value of x is not mistakenly associated with the new x
    I <<- NULL
  }
  
  # returns the current value of x, retrieved from parent env
  get <- function() x  
  
  # sets the value of I in the makeCacheMatrix env
  # to the value passed as Inverse
  setInverse <- function(Inverse) I <<- Inverse
  
  # returns the current value of I
  getInverse <- function() I
  
  # Stores the functions defined in this environment as a list with names
  # returned to the parent environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solves the inverse of the matrix stored in the makeCacheMatrix env
## and stores the result as I in the makeCacheMatrix env
## x is assumed to be a CacheMatrix object

cacheSolve <- function(x, ...) {
  # retrieves value of I from x's environment using the function
  # stored in the list within the global env
  I <- x$getInverse()
  
  # checks if I in the x env is not NULL
  # if the inverse has already been calculated, returns I without the
  # need to continue past this if statement
  if(!is.null(I)) {
    message("getting chached matrix")
    return(I)
  }
  
  # If I in the x env is NULL, solves the inverse of the matrix stored
  # in the x env and stores the result as I in the x env
  data <- x$get()
  I <- solve(data)
  x$setInverse(I)
  I
}

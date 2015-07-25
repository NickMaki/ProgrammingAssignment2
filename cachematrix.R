## Put comments here that give an overall description of what your
## functions do

#My comments (Overview of function)
# Functions to cache potentially a time-consuming computation (inversion of matrix) 
# Taking the inverse over a large matrix may take too long to compute the mean
# especially if it has to be computed repeatedly (e.g. in a loop). 
# If the contents of matrix are not changing we can cache the value of the inverse  
# rather than recomputed. This assignment takes advantage of the scoping rules in R language 

## Write a short comment describing this function
#My comments (first function description)
#The first function, makecacheMatrix creates a list containing a function to 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of matrix
#4.get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

  m.inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m.inverse <<- inverse
  getinverse <- function() m.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
#My comments (for second function) follows
# Calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets its value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {

   ## Return a matrix that is the inverse of 'x'
  m.inverse <- x$getinverse()
  if(!is.null(m.inverse)) {
    message("getting cached data")
    return(m.inverse)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m.inverse)
  m.inverse

}

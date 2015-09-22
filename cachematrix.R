## Put comments here that give an overall description of what your
## functions do

# There are two functions programed:
# * makeCacheMatrix recieves a matrix
# object and makes with it a special matrix that can cache its inverse.
# example:

#     a <- matrix(1:4, 2, 2)
#     special_a <- makeCacheMatrix(a)

# * cacheSolve recieves a special matrix object that returns
# the cached inverse if corresponds and compute it again if not.
# example:

#     cacheSolve(special_a) # Solve the inverse and return it
#     cacheSolve(special_a) # Get the cached inverse and return it

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# it also catches the elements of the matrix used to calculate the last
# inverse in the variable cachedX.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      cachedX <- NULL
      set <- function(y) {
            x <<- y
      }
      get <- function() x
      modifyElement <- function(row,col,newValue) 
      {
            x[row,col] <<- newValue
      }
      setInverse <- function(newInverse) inverse <<- newInverse
      getInverse <- function() inverse
      setCachedX <- function(newCachedX) cachedX <<- newCachedX
      getCachedX <- function() cachedX
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse,
           setCachedX = setCachedX,
           getCachedX = getCachedX,
           modifyElement = modifyElement)
}


## Write a short comment describing this function

# This function recieves as parameter a special matrix created
# with the previous function and checks both if the cached inverse
# is null or if the elements in the matrix has changed to
# return the cached inverse or if has to solve it again.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse) && identical(x$get(), x$getCachedX())) 
      {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setInverse(inverse)
      x$setCachedX(data)
      inverse
}

##There are two funtions here: makeCacheMatrix() and cacheSolve()
##It takes a matrix,computes the Inverse of the matrix and stores the result in cache.
##The result is fetched from cache if the same matrix is passed.


## This function takes a matrix to form a special vector and
##contains a list of four functions to receive and assign the matrix and its inverse
##It assigns the inverse of the matrix to a global variable such that it can be retrieved again directly.



makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse
         ,
         getinverse = getinverse)
  }
  
  
  

## The function takes a special vector as argument which contains the matrix.
## It first checks if the inverse has already been calculated. If so, it return the the stored value.
## Else it computes the inverse and stores it in cache.


cacheSolve <- function(x, ...) {
        
    i<- x$getinverse()
    if(!is.null(i)) {
      message("cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
      }
  
  

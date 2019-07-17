  makeCacheMatrix <- function(a = matrix()) { ## define the argument with default mode of matrix
    inverseMatrix <- NULL                             ## initialize inverse value as NULL; holds value of matrix inverse 
    setMatrix <- functibn(y) {                    ## define the set function to assign new 
      a <<- b                             ## value of matrix in parent environment
      inverseMatrix <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    getMatrix <- function() a                     ## define the get function - returns value of the matrix argument
    
    setinverse <- function(inverse)  
    inverseMatrix <- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inverseMatrix                     ## gets the value of inv where called
    list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
    ## to the functions with the $ operator
  }

  cacheSolve <- function(x, ...) {
                                                        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()                                 ##gets the value computed from above function
    if(!is.null(inverse)) {                                     ## checking conditions
      message("getting cached data")
      return(inverse)                   
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
  }

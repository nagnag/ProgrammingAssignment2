
##Given a matrix, this function simply constructs a cache of values related to the passed matrix, as a list.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # set value of m to NULL
      y <- NULL # set value of y to NULL
      setmat <- function(y) {
            x <<- y # set the value of x which is outside the scope of this function, hence <<-
            m <<- NULL # set the value of m which is outside the scope of this function, hence <<-
      }
      getmat <- function() x
      setinv <- function(invX) m <<- invX
      getinv <- function() m
      list(set = setmat, get = getmat, # creates a list to store the four functions
           setinv = setinv,
           getinv = getinv)
      
}


## This function gets the inverse of the matrix returned by makeCacheMatrix defined above

cacheSolve <- function(x, ...) {
      # Compare given matrix to the previous
      m <- x$getinv()
      y <- x$get() # get value of input matrix
      
      ## check if cached before
      if(!is.null(m) ){
            message("Already cached")
            return(m)
            #
      }       
      else
      {
            # if not cached before  
            message("Not cached so will perform expensive solve function here")
            m <- solve(y) # get inverse of the given matrx
            x$setinv(m) # cache the inverse
            m # return the inverse
      }
}



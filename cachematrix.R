## makeCacheMatrix
# The   function  makeCacheMatrix, creates a special "matrix", which is really a 
# list containing a function to:
        # 1 set the value of the matrix
        # 2 get the value of the matrix
        # 3 set the value of the inverse of the matrix using the solve() function
        # 4 get the value of the inverse of the matrix


makeCacheMatrix<-  function(x =  matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve   #calculates the inverse of the matrix and caches the result
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

 

# cacheSolve
# the following function calculates the inverse of the special "vector" created 
# with makeCacheMatrix. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the Solve() function.

cacheSolve<- function(x, ...) {
  m <- x$getsolve()    # Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {            
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}




this example
>  x = rbind(c(2, 4,1), c(3, 1,3), c(4, 3,1))
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2] [,3]
[1,]    2    4    1
[2,]    3    1    3
[3,]    4    3    1
> ## No cache in the first run
> cacheSolve(m)
      [,1]  [,2]  [,3]
[1,] -0.32 -0.04  0.44
[2,]  0.36 -0.08 -0.12
[3,]  0.20  0.40 -0.40

makeCacheMatrix<-  function(x =  matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

 

cacheSolve<- function(x, ...) {
  m <- x$getsolve()
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

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)) {
    message("getting cached data.")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data)
  x$setinverse(invm)
  invm
}

## How to run the program to test ##  the functions created
## >x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1       2
## [2,]  2       1
## When we run it for the first time the results are not from cache
## > cacheSolve(m)
##            [,1]          [,2]
## [1,]   -0.3333333     0.6666667
## [2,]   0.6666667     -0.3333333
## When we run the second  time the values are obtained from the ## cache
## > cacheSolve(m)
## getting cached data.
##            [,1]          [,2]
## [1,]   -0.3333333     0.6666667
## [2,]   0.6666667     -0.3333333


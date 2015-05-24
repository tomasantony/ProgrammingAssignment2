
## makeCacheMatrix function does the following things
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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


##cacheSolve function returns the inverse of the matrix. 
##If the inverse is already present it will take the 
##value from cache and skips the computation part.
##If the inverse value is not present then it will 
##compute the inverse of the matrix specified.


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


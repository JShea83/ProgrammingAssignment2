## This combination of functions will allow the inverse of a matrix to be
## cached so the "expensive" calculation of a matrix inverse will only have
## to be calculated one time. 

## This function will create a matrix object that can cache
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initialize the variable for the inverse.
  mInv <- NULL
  
  ##Assign values for the matrix and its inverse.
  set <- function(y) {
        x<<-y
        mInv<<-NULL
  }
  
  ##Get the value of the matrix.
  get<-function() x
  
  ##Assign the value of the matrix inverse.
  setInverse <- function(inverse) mInv<<-inverse
  
  ##Get the value of the matrix inverse.
  getInverse <- function() mInv
  
  ##Format functions in list.
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

## This function will compute the inverse of the matrix object created by
## the makeCacheMatrix function. If the inverse has already been computed
## and stored in makeCacheMatrix, the cached value will be retrieved.

cacheSolve <- function(x, ...) {
  
  ##Get the value of the matrix inverse if it has already been calculated.
  mInv<-x$getInverse()
  
  ##Check to see if inverse has been calculated and return cached result.
  if(!is.null(mInv)) {
    message("Getting cached data")
    return(mInv)
  }
  
  ##Get the values of the matrix.
  data <- x$get()
  
  ##Calculate the inverse of the matrix.
  mInv <- solve(data,...)
  
  ##Cache the value of the matrix inverse.
  x$setInverse(mInv)
 
  ## Return a matrix that is the inverse of 'x'
  mInv
}

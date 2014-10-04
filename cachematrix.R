## Can take a square matrix as argument.
## Generates a list of function for set the value to the matrix in an environment
## Getting the value and then setting/getting the inverse of the matrix   

makeCacheMatrix <- function(x = numeric()) {
  
  invMatrix <- NULL                   # on function initialize set matrix inverse to NULL
  set <- function(y) {                # if a new matrix is set, 
    x <<- y                           # store the matrix
    invMatrix <<- NULL                # and set its inverse matrix to NULL
  }
  get <- function() x                 # function to return the value of matrix
  setInv <- function(solve) invMatrix <<- solve # function to calculate inverse of matrix
  getInv <- function() invMatrix      # function to return the Inverse of the matrix
  list(set = set, get = get,          # a list vector with function to set,get the matrix and its 
       setInv = setInv,               # inverse values. 
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInv()           # check if Inverse has been calculated for the matrix x
  if(!is.null(invMatrix)) {         # if cached inverse is persent, 
    message("getting cached data")  # prompt on console that we are using cached value
    return(invMatrix)               # return the cahced value and exit
  }
  data <- x$get()                   # if cached value is not present, get the matrix
  invMatrix <- solve(data, ...)     # generate the inverse of the matrix
  x$setInv(invMatrix)               # store the inverse of the matrix in makeCacheMatrix environment
  invMatrix                         # printout the value of inverse of the matrix 
}



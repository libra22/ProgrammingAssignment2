#function makeCacheMatrix creates a object with 4 functions - set, get, setSolve and getSolve.
#function cacheSolve takes object created by makeCacheMatrix and attempts to retrieve the cached inverse
#matrix if available. if not, it will calculate the inverse of matrix in object using solve()


#makeCacheMatrix accepts input of matrix type and contains four functions-set, get, setSolve and getSolve
# set is to set the matrix, get is to retrieve the matrix, setSolve is to set the inverse matrix and getSolve
#is to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve= setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function
# cacheSolve accept an input of type makeCacheMatrix. If inverse of the matrix exists, it will retrieve
# from makeCacheMatrix, else, it will calculate inverse using solve() and store it in the input object 
# the output is this function is the inverse of the matrix in the input object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  return(m)
}

## script usesd to test the functions

#m <- matrix(c(3,7,0,4), nrow = 2, ncol = 2) #create matrix m 2x2
#z <- makeCacheMatrix(m) #create object z of type list with data matrix m
#z$get() #check if can retrieve matrix m via $get
#z$getSolve() #check if inverse of m exists. by right should be null as havent run solve() yet
#cacheSolve(z) #run 2nd function and see if can calc inverse
#cacheSolve(z) #rerun 2nd function to see if can get cache. must see "getting cached data"
#z$getSolve() #check if inverse is successfully stored in 1st function's object. redundant check
#z$get() %*% z$getSolve() #to see if get identity matrix i.e. solve() worked correctly

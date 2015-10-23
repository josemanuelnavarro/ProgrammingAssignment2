# This pair of functions mimics the behaviour of the example functions: the first
# one creates a special object (reminiscent to object oriented programming languages
# objects, with its public methods and such) whose values can be accesed and set.
# After creating it, calling the second function serves to calculate / load the
# inverse matrix of the original matrix supplied to the first function.

### makeCacheMatrix function
# This function takes a matrix argument (due to the exercise restrictions there is
# no input checking so don't supply any weird thing to it :-) ) and stores its value
# and creates an empty value for its inverse, while creating methods for getting
# and altering the original matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


### cacheSolve function

# This function takes a special object created by makeCacheMatrix and does two/three
# things: it first checks if the matrix's inverse exists. If it does, it just returns
# it. If if does not, it calculates it and then caches it to avoid superfluous
# computation.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

##Example (uncomment the lines to run them)

# xMatrix <- matrix(c(1,-1,0,1,1,0,1,0,1), nrow = 3, ncol = 3)
# xMatrix <- makeCacheMatrix(xMatrix)
# cacheSolve(xMatrix) #as it's not cached, it calculates it
# cacheSolve(xMatrix) #now it just fetches it and outputs it

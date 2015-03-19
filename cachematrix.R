
makeCacheMatrix <- function(x) {
  
  # Exit function if x is of type "matrix"
  if(!is.matrix(x)) stop("x must be a matrix")
  #Initialize value of m as Null
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  # Set the value of the inverse matrix
  setinverse <- function(solve) m <<- solve
  # Get the value of the inverse matrix
  getinverse <- function() m
  # Return list as function output with required variable values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Added a small function for fun to track processing time
  #     to compare processing time for calculating matrix and while retriving 
  #     for cache memory
  
  # Record system time for processing time calculation
  start.time<-Sys.time()
  # Get matrix inverse value from list
  m <- x$getinverse()
  # If matrix inverse is already calculated, get data from cached list
  if(!is.null(m)) {
    message("getting cached data")
    # Block for processing time calculation  
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message("time taken:", time.taken)
    #Time block end
    return(m)
  }
  
  # Calculate matrix inverse value if not available in cache memory
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # Block for processing time calculation 
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message("time taken:", time.taken)
  #Time block end
  m
}

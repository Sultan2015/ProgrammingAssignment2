# Here I provide both functions needed for the 2nd R programming assignment.
# Both functions have checks for 1) missing argument, 2) non-matrix argument, 2) not square matrix.
# This is done in case someone will use them separately (for a debugging, for example).

# The 1st function gets the inverse of the matrix provided as an argument.
# After that, both the argument and the solution are stored in the cache.

makeCacheMatrix <- function(x) {
  
  if (missing(x))                                       # argument check
  {
    warning("Please provide an argument")
    return(NA)
  }
  
  if (!is.matrix(x))                                    # matrix argument check
  {
    
    warning("The argument is not a matrix!")
    return(NA) 
  }
  
  if (ncol(x)!=nrow(x))                                 # square matrix argument check
  {
    warning("The matrix is not square!")
    return(NA) 
  }
  
  y<-solve(x)                                           # getting the inverse matrix
  cachematrix<<-x                                       # storing the matrix in the cache
  cacheinverse<<-y                                      # storing the inverse in the cache
  return(y)                                             # returning the result
}

# The 2nd function chects whether 1) the cache exists, 2) the argument coincides with cached matrix.
# If both answers are "yes", the function retreives the inverse from the cache.
# Otherwise the function calculates the inverse from the scratch using the previous function.

cacheSolve <-function(x) {
  
  if (missing(x))                                       # argument check
  {
    warning("Please provide an argument")
    return(NA)
  }
  
  if (!is.matrix(x))                                    # matrix argument check
  {
    
    warning("The argument is not a matrix!")
    return(NA) 
  }
  
  if (ncol(x)!=nrow(x))                                 # square matrix argument check
  {
    warning("The matrix is not square!")
    return(NA) 
  }
  
  if (exists("cachematrix") & exists("cacheinverse"))   # looking for the cache
  {
    if (identical(x,cachematrix))                       # looking for the argument in the cache
    {
      y<-cacheinverse                                   # retreivung the inverse from the cache
      print("Cool! We already have this inverse in the cache.")
      return(y)
    }
  }
  
  y<-makeCacheMatrix(x)                                 # solving from the scratch using the previous function
  print("No cache exists for this matrix! We had to find a new inverse.")
  return(y)
}
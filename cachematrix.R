#There might be some benfit in terms of speed and memory, if cache the inverse of matrix
#because matrix inversion is a costly computation. The complexity of matrix inversion is O(n^3) using Guass- Jordan elimination method.

#makeCacheMatrix defined below to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
# and finally the function creates a list to hold above four values


makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL     #initializing inverse_matrix value to NULL
  set_matrix <- function(y){ # function to set values
    x <<- y                  # setting x value to input matrix
    inverse_matrix <<- NULL  # setting cached inverse_matrix to NULL
  }
  get_matrix <- function() x  #getting the input matrix 
  set_inverse <- function(inverse) inverse_matrix <<- inverse #setting inverse of matrix
  get_inverse <- function() inverse_matrix   #getting inverse of matrix
  list(set = set_matrix, get = get_matrix,
       setinverse = set_inverse,
       getinverse = get_inverse) #list to hold set and get of both input matrix and inverted matrix
}

#cacheSolve funtion returns the inverse of the matrix that passed to the above function.It first checks the inverse has already been computed. If so, the gets the result from cache and skips the computation. If not, it computes the inverse and sets the value in the cache using setinverse function

cacheSolve <- function(x, ...) {
        
          inverse_matrix <- x$getinverse() #getting matrix inverse and assign to inverse_matrix
          if(!is.null(inverse_matrix)){  #checking the value of inverse_matrix, if it is cahced or not
            message("getting cached data")
            return(inverse_matrix) #if inverse_matr holds a value other than null then return that value, which is cache value
          }
          data <- x$get() #if inverse_matrix is not chached i.e., if inverse_matrix = NULL read the input matrix to compute inverse of matrix
          inverse_matrix <- solve(data) #compute inverse of matrix using solve()
          x$setinverse(inverse_matrix) #setting inverse of matrix
          inverse_matrix #retruning inverse of matrix
}

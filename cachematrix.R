  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Write a short comment describing this function
 
 #makeCacheMatrix  creates special matrix object that can cache its inverse
 #The function contains set, and get the value of matrix 
 #Also, set, and get the value of the inverse
  
  makeCacheMatrix <- function(x = matrix()) {
 
# inverse matrix value to NULL
 inversematrix <- NULL
       set <- function(y) 
 	{
              x <<- y
              inversematrix <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) inversematrix <<- solve
       getinverse <- function() invx
# return a list of all the above functions
       list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
  
  
  ## Write a short comment describing this function
 #The following function calculates the inverse of the matrix by using makeCacheMatrix
 #it also checks if the matrix inverse is already calculated or not
 #if yes then it gets the inverse from the cache and skips the computation 
 #Otherwise, it calculates the inverse of the matrix and 
 #sets the value of the inverse in the cache via the setinverse function. 
 
  
  cacheSolve <- function(x, ...) {
      inversematrix <- x["getinverse()"]
       if(!is.null(inversematrix)) 
 	{
              message("getting cached inverse Matrix")
              return(inversematrix)
       }
       datamatrix <- x$get()
       invs <- solve(datamatrix, ...)
       x$setinverse(inversematrix)
       inversematrix
  }

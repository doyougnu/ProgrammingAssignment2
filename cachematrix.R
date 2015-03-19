#functions create special matrix wrapper that allows for caching the inverse
#of the given matrix

#makeCacheMatrix accepts a matrix and returns a matrix of functions to get
# or set the matrix to be operated on, or get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  #function takes a matrix and caches the inversion
  inv <- NULL
  
  setMat <- function(y){
    myMatrix <<- y
    inv <<- NULL
  }
  
  getMat <- function() x
  setInv <- function(matInv) inv <<- matInv
  getInv <- function() inv
  
  return(matrix( data = list(setMat = setMat
                             , getMat = getMat
                             , setInv = setInv
                             , getInv = getInv)
                 , 2, 2))
}

#cacheSolve accepts the matrix of functions returned by makeCacheMatrix, checks
#for the cached inverse, if found returns the inverse, if not the computes the inverse
#and returns it
cacheSolve <- function(x, ...){
  
  inv <- x[[2, 2]]()
  
  if (!is.null(inv)) {
    message("retrieving cached inverse")
    return(inv)
  }
  else {
    
    #copy the matrix, i think this is dangerous in real code as the functions
    #or function indices may have changed
    matCopy <- x[[2,1]]()
    message("computing inverse")
    
    #this code should really be in setInv from the first function but doing it
    #this way comports with the project requirements
    retInv <- solve(matCopy)
    x[[1, 2]](retInv)
    return(retInv)
  }
}
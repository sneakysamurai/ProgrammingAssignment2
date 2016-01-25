## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (original = matrix()){
  
  evildoppleganger <- matrix(nrow = nrow(original), ncol = ncol(original))
  
  set <- function (redo){
    
    original <<- redo
    evildoppleganger <<- matrix(nrow = nrow(original), ncol(original))
  }
  
  get <- function() {
    original
  }
  
  setinverse <- function (inverse) {
    evildoppleganger <<- inverse
  }
  
  getinverse <- function (){
    evildoppleganger
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}




cachesolve <- function(newmatrix, ...){
  
  inverse <- newmatrix$getinverse()
  
  if(!is.na(inverse[1,1])){
    ## had to look at a variable to see if it was NA instead of the entire matrix
    message("getting cached data")
    return(inverse)
  }
  
  else{
    data <- newmatrix$get()
    inverse <- solve(data)
    newmatrix$setinverse(inverse)
    inverse 
  }
}
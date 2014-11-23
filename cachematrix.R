## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function allows us to cache an inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ##m will be the inverse matrix It resets with NULL
  m<-NULL 
  
  ##Sets the Matrix
  set <- function(matrix){ 
    x<<-matrix    
    m<<-NULL   
    
  }
  
  ##gets the matrix
  get<-function() {
    x 
  }
  
  ##This will be called by cacheSolve the first time 
  setInverse<-function(inverse){
    m<<-inverse 
                               
  }
  
  ##This will be called by cacheSolve after the first attempt.
  getInverse<-function(){ 
    m
  }
  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##x is from makeCacheMatrix
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){  ##if mean was already cached return 
    ##the following message and m
    message("getting cached data") ##gives message
    return(m)  ##returns m
  }
  data <- x$get() ##this is run if m returns NULL
  m <- solve(data, ...) ##calculates m if it was NULL
  x$setInverse(m)  ##stores the inverse object
  return(m) ##returns the new inverse matrix
}



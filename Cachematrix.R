##first we have to make a matrix and we will use makeCacheMatrix 
## to create and list a matrix that we find the inverse of.
##The function will create a square matrix that we can take the 
##inverse of. 
makeCacheMatrix <- function(x = matrix()) {
  #stores the cached matrix values
  inv<-NULL
  #creates the matrix 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  #gets the value of the matrix
  get<-function()x
  #inverts the matrix and stores it
  setinverse<-function(inverse) inv<<-inverse
  #gets the inverted matrix
  getinverse<-function()inv
  #returns the created function
  list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}

##cacheSolve will calcuates the inverse of the matrix
##If the inverted matrix doesn't exist in cache, it will
##be created in teh working environment and it's inverted
##matrix will be stored in inv
cacheSolve<-function(x,...){
  #attempt to the get the inverse of the matrix
  inv<-x$getinv()
  #returned and inverted matrix if it exists
  #if not a new matrix will be created in working envrionment
  if(!is.null(inv)){
    message("inverse is cached")
    return(inv)
  }
  #computes the inverse of a matrix
  m<-x$get()
  inv<-solve(m,...)
  #cache the inverse matrix
  x$setinv(inv)
  #shows the inverse of the matrix
  return(inv)
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setslove<- function(solve) m<<- solve
  getslove<- function() m 
  list(set = set, 
       get = get,
       setslove = setslove,
       getslove = getslove)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         m<- x$getslove()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  data<- x$get()
  m<- solve(data,...)
  x$setslove(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

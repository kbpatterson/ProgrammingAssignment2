## The function below creates a list of length 4, which has functions as its elements
## The set function sets the values of matrix
## The get function gets the values of the matrix
## The set_inverse function using solve() to set the inverse of the matrix
## The get_inverse function gets the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){
    x
  } 
  set_inverse<-function(solve){
    i<<-solve
  } 
  get_inverse<- function(){
    i
  } 
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)

}


## The function below calculates the inverse of the matrix created above
## The argument x in cacheSolve() must be of type makeCacheMatrix
## It checks to see if the inverse has already been calculated
## If so, it grabs the inverse from the cache
## If not, it calculates the inverse and sets the value of the inverse in the cache using the set_inverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$get_inverse()
  if(!is.null(i)){
    message("retrieving cached data")
    return(i)
  }
  d<-x$get()
  i<-solve(d,...)
  x$set_inverse(i)
  i
}

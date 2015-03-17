## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## input: takes a square matrix
## goal: find its inverse and cache
## return: inverse matrix
## output format: list of named methods
### get: will return the matrix input data
### set: will set the matrix input data
### setimat: will set the inverse matrix result (cache)
### getimat: will return the inverse matrix result from cache
### example: x<-matrix(rexp(16), 4,4) ix<-makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
  imat<-NULL
  get<-function()x
  set<-function(y){
    x<<-y
    imat<-NULL
  }  
  setimat<-function(invmat)imat <-invmat
  getimat<-function()imat
  list(get=get,set=set,getimat=getimat,setimat=setimat)

}


## Write a short comment describing this function
## cacheSolve return an inverse of the specail "matrix"  object returned by makeCacheMatrix funtion
## input: special "matrix" object returned by makeCacheMatrix function
##  goal: return inverse of a  matrix  from cache, if not found find inverse and cache.
## return: inverse matrix
## output format: matrix
### example: cacheSolve(ix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  imat <-x$getimat()
  if(!is.null(imat)){
    return( imat)
    
  }
  data<-x$get()
  imat<- solve(data,...)
  x$setimat(imat)
  imat
        
}

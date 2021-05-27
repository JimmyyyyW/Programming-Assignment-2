#Assignment 2 Jimmy-H
#Caching the Inverse of a Matrix 
makeCacheMatrix<-function(y=matrix()){
  inv<-NULL
  set<-function(matrix){
    y<<-matrix
    inv<<-NULL
  }
  get<-function(){y}
  set_Inverse<-function(inverse){inv<<-inverse}
  get_Inverse<-function(){inv}
  list(set=set, get=get, set_Inverse=set_Inverse, get_Inverse=get_Inverse)
}

CacheSolve<-function(x,...){
  inv<-x$get_Inverse()
  if(!is.null(y)){
    message("getting cached data")
    return(y)
  }
  mat<-x$get()
  inv<-solve(mat, ....)
  x$set_Inverse(y)
  y
}
##Armazenando o inverso de uma matriz
#criar a matriz especial
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y  ##<<- atribui valor
    i <<- NULL
  }
  get <- function() x  ##vai criar a inversa
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##Esta função calcula o inverso da "matriz" especial retornada makeCacheMatrixacima. 
  ##Se o inverso já tiver sido calculado (e a matriz não tiver sido alterada), 
  ##você cacheSolvedeverá recuperar o inverso do cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("obtendo dados")  ##se já existe retorna mensagem
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

library("ggplot2")
library("rSymPy")
library("pracma")
library("polynom")

x <- c(50, 80, 110, 140, 170)
y <- c(3.5, 4.2, 5.7, 3.8, 1.2)

dat <- data.frame(cbind(x, y))

lagrange.poly <- function(x, y) {
  
  l <- list() 
  k <- 1
  
  for (i in x) {
    num <- 1
    denom <- 1
    
    p <- x[! x %in% i]
    
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = ", collapse = ")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = ", collapse = ")
    }

    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = ", collapse = ")
    k <- k + 1
  }
  
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = ", collapse = ")
  }
  
  x <- Var('x')

  return(sympy(paste("simplify(", eq, ")")))
}

poly.calc(x, y)

f <- function(x) {
  return(37.38272 - 1.574414*x + 0.02508796*x^2 - 0.0001608025*x^3 + 3.549383e-07*x^4 )
}

ggplot(dat, aes(x=x, y=y)) + 
  geom_point(size=5, col='blue') + 
  stat_function(fun = f, size=1.25, alpha=0.4)

x <- c(50, 80, 110, 140, 170)
fx <- c(3.5, 4.2, 5.7, 3.8, 1.2)


finite.differences <- function(x, y) {
  if (length(x) != length(y)) {
    stop('Ambos vectores deben ser iguales')
  }
  n <- length(x)
  
  fdx <- vector(length = n)
  
  for (i in 2:n) {
    fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }

  fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  return(fdx)
}


finite <- finite.differences(x, fx)
finite

dx <- c(0.02333333,  0.05000000, -0.06333333, -0.08666667, -0.08666667)

finite <- finite.differences(dx, fx)
finite



library(numDeriv)
x <- c(1)

f <- function(x) {
  return(x*exp(x))
}


dos.puntos <- function(f, x) {
  steps <- c(0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001)
  n <- length(steps)
  fdx <- vector(length = n)
  for (h in 1:n) {
    fdx[h] <- (f(x + 0.5 * steps[h]) - f(x - 0.5 * steps[h])) / steps[h]
  }
  return(fdx)
}

tres.puntos <- function(f, x) {
  steps2 <- c(0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001)
  n2 <- length(steps2)
  fdx2 <- vector(length = n2)
  for (h in 1:n2) {
    fdx2[h] <- ((-3*f(x)) + (4*f(x + 0.5 * steps2[h])) - f(x + steps2[h])) / steps2[h]
  }
  return(fdx2)
}

for (i in x) {
  print(dos.puntos(f, i))
}
#Para la formula a dos puntos el h pierde precicion luego de que este es 0.001 puesto que
#despues de superar dicha h los valores de fdx se vuelven constantes.

for (i in x) {
  print(tres.puntos(f, i))
}
#Para la formula a tres puntos el h pierde precicion luego de que este es 0.0001 puesto que
#despues de superar dicha h los valores de fdx se vuelven constantes.

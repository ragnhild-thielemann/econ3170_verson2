library(tidyverse)

funk <- function(x){
  return (1-x**2)
}

fou <- function(x,N){
  verdi = 0
  for (k in -N:N){
    if (k == 0){
      verdi = verdi + 1 - pi**2/3
    } else {
      ledd = (-1)**k/k**2
      verdi = verdi - 2*ledd*exp(1i*k*x)
    }
  }
  return (Re(verdi))
}


x_verdier = seq(-pi,pi,0.001)
N = 100
t <- tibble(x = x_verdier, f = funk(x_verdier), fo = fou(x_verdier,N))


ggplot(t) +
  geom_line(aes(x = x , y = f)) + 
  geom_line(aes(x = x , y = fo))

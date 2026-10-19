
library(tidyverse)
funk <- function(x){
  return(exp(abs(x)))
}
funk(1)
fou <- function(t,N){
  
  verdi <- 0 
  for (k in -N:N){
    if (k == 0){
      faktor <- ((exp(pi)*(-1)**k)-1)/(pi*(1+k**2))
      verdi = verdi  + faktor
    } else{
      faktor <- ((exp(pi)*(-1)**k)-1)/(pi*(1+k**2))
      ledd <- faktor *exp(1i * t * k)
      verdi = verdi + ledd
    }
  }
  return(Re(verdi))
}
error = 0.01
print(fou(0,100))
x_verdier <- seq(-pi + 0.01,pi-0.01,0.001)
x_verdier_eksp <- seq(-pi + error,pi-error,0.01)


t <- tibble(x = x_verdier_eksp,f = funk(x_verdier_eksp), a = fou(x_verdier_eksp,10))

t$a
ggplot(t) +
  
  geom_line(aes(x,f)) + 
  geom_line(aes(x,a))
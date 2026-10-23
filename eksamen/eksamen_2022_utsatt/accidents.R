
library(tidyverse)


data <- read_csv("eksamen/eksamen_2022_utsatt/accidents.csv")
View(data)
L <- function(a,b,y,x){
  total_sum = 0
  for (x_i in x){
    for (y_i in y){
      total_sum <- total_sum + y_i*(a + b*x_i) - exp(a + b*x_i)
    }
    
    

    
  }
  return(total_sum)
}

a = 1 ; b = 0.5

der <- function(funk,h,a,b){
  svar <-( L(a+h,b,data$speed,data$accidents) - L(a,b,data$speed,data$accidents))/h
  return(svar)
  }
der(L,0.1,a,b)

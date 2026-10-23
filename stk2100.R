

library(tidyverse)
library(tidymodels)



url <- "http://web.stanford.edu/~hastie/CASI_files/DATA/leukemia_big.csv"

data <- read_csv(url)

View(data)

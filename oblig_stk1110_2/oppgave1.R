
library(tidyverse)
library(janitor)
nettside <- "https://www.uio.no/studier/emner/matnat/math/STK1110/data/"
url <- paste0(nettside,"kveite_for.txt")

#gjør det til en tibble
kveite <- tibble(read.table(url,header = TRUE))|>
  janitor::clean_names()

boksplott <- kveite |>
  ggplot(aes(x = fortype, y = vekt)) +
  geom_boxplot(aes(group = fortype)) + 
  labs

boksplott

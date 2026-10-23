

library(readxl)
library(lubridate)
library(janitor)
library(tidyverse)
brian <- read_excel("eksamen/eksamen_2024/subscribers (1).xlsx")|>
  janitor::clean_names()|>
  mutate(start = as.Date(start),end = as.Date(end))

april <- brian |>
  filter(ymd("2024-04-15") %within% interval(start,end))|>
  summarise(antall = sum(number))

print(paste("Han ma bake", april$antall , "brod den 15 April"))

days <- seq.Date(ymd("2024-01-01"),ymd("2024-12-31"),"1 day")
brod <- vector("integer",length = length(days))


for (i in 1:length(brod)){
  d = days[i]
  brod_antall<- (brian |>
    filter(ymd(d) %within% interval(start,end))|>
    summarise(antall = sum(number)))$antall
  
  brod[i] = brod_antall
}

data_brod <- tibble(day = days, brod = brod)

ggplot(data_brod)+
  geom_line(aes(x = days, y = brod))

#pakker ut alle filene i listen

a <- unzip("eksamen/eksamen_2024/subscribers (1).zip")

N <- length(a)
character
name_vek <- vector("character",N)
number_vek <- vector("integer",N)
start_vek <- c()
end_vek <- c()

for (i in 1:N){
  fil <- a[i]
  les <- read_excel(fil)
  name_vek[i] <- les[4,2]$...2 
  number_vek[i] <- as.numeric(les[5,2]$...2)
  start_vek <- c(ymd(ymd("1899-12-31") + (as.numeric(les[7,2]$...2))),start_vek)
  end_vek<- c(ymd(les[7,5]$...5),end_vek)
}


t <- tibble(name_vek, number_vek,start_vek,end_vek)
t
ymd("1899-12-31") + 45405
lubridate::as_date(45405)
as.Date(19879)



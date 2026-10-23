

library(tidyverse)
library(tidymodels)
library(workflows)
library(recipes)

url <- "https://www.uio.no/studier/emner/matnat/math/STK2100/data/"

nuclear <- tibble(read.table(paste0(url,"nuclear.dat",sep = ""),
                      header = TRUE))|>
  
  #kontrollerer at alt er tall
  mutate(across(everything(),as.double)) |>
  mutate(cost = log(cost))|>
  select(-c(t2,bw,t1,pr,cum.n,ct,ne))

rec <- recipe(cost~.,
              data = nuclear)

lm_model <- linear_reg()|>
  set_engine("lm")

wf <- workflow()|>
  add_model(lm_model)|>
  add_recipe(rec)

fit_wf <- fit(wf,nuclear)
tidy(workflows::extract_fit_engine(fit_wf) )|>
  arrange(p.value)

glance(workflows::extract_fit_engine(fit_wf) )
  


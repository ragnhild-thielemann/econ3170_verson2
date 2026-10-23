
#a)
library(tidyverse)
library(broom)
library(workflows)
library(tidymodels)
library(recipes)
library(parsnip)
library(xgboost)
#for å kunne reprodusere, setter vi en seed
set.seed(45)


forbes <- read_csv("eksamen/eksamen_2022_utsatt/forbes_2022_billionaires.csv")
forbes$age
oldst <- max(forbes$age,na.rm = TRUE)
young <- min(forbes$age,na.rm = TRUE)

print(paste("Den eldste er",oldst, "ar, mens den yngste er ",young,"ar."))

print(paste("Den hoyeste formuen i verden er",max(forbes$finalWorth)))

print(paste("Gjennomsnittlig formue blandt de 100 rikeste er", (forbes |>
              arrange(desc(finalWorth))|>
              slice(1:100)|>
              summarise(f = mean(finalWorth)))$f, "millioner USD dollars"))
      

#b)

land <- forbes |>
  summarise(antall = n(), .by = country)|>
  arrange(desc(antall))|>
  slice(1:12)|>
  mutate(country = as_factor(country))


p1 <- ggplot(land)+
  geom_col(aes(x = country, y = antall)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45,margin = margin(t = 15))) 
  

#c)


forbes |>
  arrange(rank)|>
  slice(1:200)|>
  ggplot(aes(x = rank, y = finalWorth)) + 
  geom_line()

men_wealth <- forbes|>
  filter(gender == "M")|>
  arrange(rank)|>
  slice(1:200)|>
  mutate(rank = row_number())|>
  select(men = finalWorth,rank)
 

woman_wealth <- forbes |>
  filter(gender == "F")|>
  arrange(rank)|>
  slice(1:200) |>
  mutate(rank = row_number())|>
  select(woman = finalWorth,rank)

total_wealth <- men_wealth |>
  left_join(woman_wealth, by = join_by(rank))|>
  pivot_longer(cols = -rank,
               names_to = "gender",
               values_to = "wealth")


ggplot(total_wealth)+
  geom_line(aes(x = rank, y = wealth, color = gender))

#d)

jap <- forbes |>
  mutate(old = if_else(age>65,1,0))|>
  mutate(kategori = case_when((age>65 & gender == "F")~"old_woman",
                              (age<65 & gender == "F")~"young_woman",
                              (age>65 & gender == "M")~"old_man",
                              (age<65 & gender == "M")~"young_man",
                              TRUE ~ "ikke_definert"))|>
  select(kategori,country)|>
  summarise(antall = n(), .by = c(country,kategori))|>
  pivot_wider(names_from = kategori,
              values_from = antall,
              values_fill = 0)

#e)


birth <- forbes |>
  filter(month(birthDate) != "1" | day(birthDate) != "1")|>
  mutate(week = week(birthDate))|>
  summarise(antall = n(), .by = c(week,country))


ggplot(birth) +
  geom_col(aes(x = week, y = antall))



#2.

a <- colnames(forbes|>
                select(age,finalWorth,category,country,selfMade,gender))
antall <- vector("integer",length(a))
for (i in 1:length(a)){
  kat <- a[i]
  
  #brukes for å hente ut av en liste
  antall[i] = sum(is.na(forbes[[kat]]))
}

t <- tibble(a,antall)
#Tibble som viser oversikt over tomme variabler
t

#fjerner alle observasjoner der det mangler noe
manglende <- forbes |>
  select(age,finalWorth,category,country,selfMade,gender)|>
  drop_na() 

flere_enn_ti <- manglende|>
  summarise(antall = n(), .by = country)|>
  filter(antall > 10)

t <- manglende|>
  mutate(country = as.factor(country))|>
  mutate(country = if_else(country %in% flere_enn_ti$country,country,"other"))|>
  #gjør true/false om til integers
  mutate(selfMade = as.integer(selfMade))

splittet <- initial_split(t,0.75)
tren <- training(splittet)
test <- testing(splittet)


rec <- recipe(finalWorth~., tren)|>
  step_dummy(all_nominal_predictors())
  #normaliserer, slik at vi måler i samme måleenhet


xg <- boost_tree()|>
  set_mode("regression")|>
  set_engine("xgboost")

rd <- rand_forest()|>
  set_mode("regression")|>
  set_engine("ranger")
linar <- linear_reg()|>
  set_engine("lm")

wf_lin <- workflow()|>
  add_model(rd)|>
  add_recipe(rec)

wf_xg <- workflow()|>
  add_recipe(rec)|>
  add_model(xg)



fit_lm <- fit(wf_lin, tren)
pred_lm <- predict(fit_lm, test)

fit_xg <- fit(wf_xg,tren) 
pred_xg <- predict(fit_xg,test)

rmse <- tibble(xg_error = pred_xg$.pred -test$finalWorth,lm_error = pred_lm$.pred-test$finalWorth)|>
  summarise(xg_error = mean(xg_error**2),lm_error = mean(lm_error**2))

print(rmse$xg_error-rmse$lm_error)
rmse


ggplot(tren, aes(x = finalWorth)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  theme_bw()



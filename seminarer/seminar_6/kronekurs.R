library(tidyverse)
library(tidymodels)
library(workflows)
library(recipes)
set.seed(67)
url <- paste0(
  "https://data.norges-bank.no/api/data/EXR/B.EUR.NOK.SP?format=csv",
  "&startPeriod=2022-11-24",
  "&endPeriod=2023-11-24",
  "&locale=en"
)

dx <- read_delim(url, delim = ";")|>
  rename(date = TIME_PERIOD, rate = OBS_VALUE)|>
  select(date,rate)|>
  mutate(day = row_number())|>
  arrange(date)
View(dx)
dx
p1 <- dx |>
  ggplot(aes(x = date, y = rate)) +
  geom_line()


#Vi deler inn i treningsett og testsett
split <- initial_time_split(dx,0.8) #deler inn i 80% og 20%
trening <- training(split)
test <- testing(split)

nrow(trening)
nrow(test)

lm_model <- linear_reg()|>
  set_engine("lm")
#Tilnærmer en modell med et femtegradspolynom
rec <- recipe(rate~day, data = trening)|>
  step_poly(day,degree = 5)

work <- workflow()|>
  add_model(lm_model)|>
  add_recipe(rec)

#Testdelen skal være 0.2, som dermed er 5 folder
N = 5; folds <- vfold_cv(trening,v = N)

resultater <- vector("list", N)

for (i in 1:N){
  test_fold <- rsample::assessment(folds$splits[[i]])
  trening_fold <- rsample::analysis(folds$splits[[i]])
  
  fit_wf <- fit(work,trening_fold)
  pred <- predict(fit_wf,test_fold)
  resultater[[i]] <- tibble(
    day = test_fold$day,
    faktisk = test_fold$rate,
    predikert = pred$.pred,
    feil = pred$.pred - test_fold$rate,
    fold = i
  )
  
}
resultater <- bind_rows(resultater)



#jobber med en liste av tibbels
resultater |>
  summarise(
    RMSE = sqrt(mean(feil^2)),
    R2 = 1 - sum(feil^2) /
      sum((faktisk - mean(faktisk))^2)
  )
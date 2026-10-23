
library(tidyverse)
library(tidymodels)
library(workflows)
library(recipes)
#Vi lagrer linken som et objekt
url <- paste0(
  "https://data.norges-bank.no/api/data/EXR/B.EUR.NOK.SP?format=csv",
  "&startPeriod=2022-11-24",
  "&endPeriod=2023-11-24",
  "&locale=en"
)

#Ved å bruke read_delim har vi mer kontroll. Der deles det bare der vi ønsker det
dx = read_delim(url, delim = ";") |>
  select (date = TIME_PERIOD, rate = OBS_VALUE)|>
  
  #sorterer på dager som det første vi gjør, før vi tar å setter dager til dagnummeret
  arrange(date)|>
  mutate(day = row_number())

ggplot(dx) +
  geom_line(aes(date,rate))

#2.

# Deler dataen i et treningsett og et testsett
# Det er en avheninghet over  tid

#Splitten er determinsitisk, så vi trenger ikke sette seeden, da den splittes på datoen
fx_split <- rsample::initial_time_split(dx,prop = 0.8)

fx_train <- rsample::training(fx_split)
fx_test <- rsample::testing(fx_split)

# 3. Resamping, CV


lm_spec <- linear_reg()|>
  set_engine("lm")

poly_rec <- recipe(rate~day, 
                   data = fx_train)|>
  
  #variablen er day, som det skal regnes polynomer for
  step_poly(day,degree = 5)

poly_wf <- workflow()|>
  add_recipe(poly_rec)|>
  add_model(lm_spec)


#Setter seed for resampling
set.seed(67)

#Lager bare folds på treningsdataen
random_folds <- vfold_cv(fx_train, v = 10)

time_folds <- sliding_window(fx_train, 
                             lookback = Inf,
                              assess_stop = 20,
                             step = 20, 
                             skip = 99)


poly_wf |>
  fit_resamples(
    resamples = time_folds
  )|>
  collect_metrics()


poly_wf |>
  fit_resamples(
    resamples = random_folds
  )|>
  collect_metrics()

# tilpasse dataen 

poly_fit <- poly_wf |>
  fit(data = fx_train)


poly_fit |>
  augment(fx_test) |>
  rsq(truth = rate, estimate = .pred)


poly_fit |>
  augment(dx)|>
  ggplot() +
  geom_line(aes(x = date,y = rate,color = "Actual"))+
  geom_line(aes(x = date,y = .pred, color = "Predicted")) +
  geom_vline(xintercept = min(fx_test$date))


fx_lag <- dx|>
  mutate(rate_lag = lag(date))|>
  
  drop_na(rate_lag)

fx_lag_split <- initial_time_split(fx_lag,0.8)

test_lag <- testing(fx_lag_split)
tren_lag <- training(fx_lag_split)

lag_wf <- workflow()|>
  add_formula(rate~rate_lag)|>
  add_model(lm_spec)

lag_folds <- sliding_window(tren_lag, 
                            lookback = Inf,
                            assess_stop = 20,
                            step = 20, 
                            skip = 99)

lag_wf |>
  fit_resamples(lag_folds)|>
  fit(data = test_lag)
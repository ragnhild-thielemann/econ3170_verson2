

library(tidyverse)
library(tidymodels)
library(workflows)
library(recipes)
library(broom)

#pakke for lasso
library(glmnet) 


#bruker delim, da jeg spesifiserer at cellene er delt med ;
data <- read_delim("seminarer/seminar_6/student-mat.csv", delim = ";")|>
  rename(grades = G3)|>
  select(grades,age,Mjob,Fjob,traveltime,studytime,failures,absences,G1,G2)|>
  
  #gjør alle strenger til faktorer. Tall beholdes som doubles,
  mutate(across(where(is.character), as.factor))

data
#Vi deler dataen før vi har begynt å behandle den. Vi oppretter dog en workflow, som vi sender begge datasettene våre igjenom
splittet <- initial_split(data,0.8)

trening <- training(splittet)
test <- testing(splittet)




#preposeccing

rec <- recipe(grades~ ., 
              data = trening)|>
  
  #gjøres til dummy, slik at hver av dem er en prediktor
  step_dummy(all_nominal_predictors())

rec|>
  prep()|>
  bake(new_data=NULL)


# Vi ser at vi har 7 prediktorer og 1 outputvariabel

linear <- linear_reg()|>
  set_engine("lm")

lasso <- linear_reg(penalty = 0.1, #penalty = 0.1
                    mixture = 1)|> #mixture = 1 - en full linjær modell
  set_mode("regression")|>
  set_engine("glmnet")


work_lm <- workflow() |>
  add_model(linear)|>
  add_recipe(rec)

work_lasso <- workflow() |>
  add_model(lasso)|>
  add_recipe(rec)

fit_lm <- fit(work_lm,trening)
fit_lasso <- fit(work_lasso,trening)


pred_lm <- predict(fit_lm,test)$.pred
pred_lasso<- predict(fit_lasso,test)$.pred


tabell <- tibble(error_lm = test$grades-pred_lm, error_lasso = test$grades-pred_lasso)



ggplot(tabell)+
  geom_density(aes(x = error_lm)) + 
  geom_density(aes(x = error_lasso))

total_error <- tabell |>
  summarise(error_lm = mean((error_lm-mean(error_lm))**2), error_lasso = mean((error_lasso-mean(error_lasso))**2))


total_error

N <- 15 ; folds <- vfold_cv(trening,N)


# Vi skal nå tune en penalty basert på kryssvalideing av dataen.
lasso_tune <- linear_reg()|>
  set_engine("glmnet")|>
  set_args(penalty = tune(),
           mixture = 1)
rec <- recipe(grades ~.,
              data = trening)|>
  step_dummy(all_factor_predictors())

work <- workflow()|>
  add_model(lasso_tune)|>
  add_recipe(rec)

lambda_grid <- tibble(penalty = 10**seq(-3,0.5,length.out = 10))
grid <- tune_grid(lasso_tune,rec,resamples  = folds,grid = lambda_grid)

n <- select_best(grid,metric = "rmse")
n

#setter inn verdien for a i workflowen

work <- finalize_workflow(work, n) 

fit_lasso_mod <- fit(work, trening)
pred <- predict(fit_lasso_mod,test)

a <- tibble(error = pred$.pred-test$grades)|>
  summarise(error = mean(error**2))

pred_lm <- predict(fit_lm, test)$.pred
pred_lasso_tuned <- predict(fit_lasso_mod, test)$.pred

tibble(
  modell = c("LM", "Lasso tunet"),
  MSE = c(
    mean((test$grades - pred_lm)^2),
    mean((test$grades - pred_lasso_tuned)^2)
  ),
  RMSE = c(
    sqrt(mean((test$grades - pred_lm)^2)),
    sqrt(mean((test$grades - pred_lasso_tuned)^2))
  )
)


extract_fit_parsnip(fit_lm)

extract_fit_parsnip(fit_lasso)

#kan pipe på samme måte som vi gjør i tidyverse med broom
tidy(extract_fit_parsnip(fit_lm)) |>
  arrange(desc(abs(estimate)))
tidy(extract_fit_parsnip(fit_lasso_mod))|>
  arrange(desc(abs(estimate)))

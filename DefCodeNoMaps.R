library (tidyverse)
library(janitor)
library(ggplot2)
library(recipes)
library(tidymodels)
library(readr)
library(devtools)
library(vdemdata)
library (parsnip)
library(mlbench)
library(xgboost)
library(parsnip)
library(parallel)
library(mice)
library(maps)
library(vip)
###some unused as for now


####data cleaning and merging datasets------
vdem <- vdem %>%
  filter(year %in% 1960:2021) %>%
  select(country_name, country_text_id, year, v2x_polyarchy)




WDIData <- read_csv("//hume/student-u74/augustom/pc/Downloads/WDIData.csv")
View(WDIData)


names(WDIData) <- str_replace_all(names(WDIData), c(" "="_",","=""))

WDIData <- WDIData %>%
pivot_longer(
  cols = c("1960":"2021"),
  names_to = "year",
  values_to = "value"
)

WDIData <- WDIData[c(1,2,4,5,6)]

WDIData <- WDIData %>%
  pivot_wider(
    names_from = "Indicator_Code",
    values_from = "value"
  )

WDIData <- WDIData %>%
  subset(year<2021 & year>1989)


WDIData <- WDIData%>%
  rename(country_text_id = Country_Code)

WDIVdem <- merge(vdem,WDIData,by=c("year","country_text_id"))



WDIVdem <- WDIVdem[-c(2,5)]



WDIVdem <- WDIVdem %>%
  unite("country_year", country_name:year,sep = "_")

row.names(WDIVdem) <- WDIVdem[,1]
WDIVdem <- WDIVdem[-1]
###cleaning the complete dataset-----------
###some variables are not stored at all: we filter excluding them, and we also exclude the variables that have more than 10% missing observations
WDIVdem <- WDIVdem %>% 
  select(where(~mean(is.na(.))< 0.1))

###MICE imputes missing values, there is a randomness assumption that we cannot verify, but we deleted variables with systematic missing at least
#### We chose the CART (classification and regression trees, in this case regression, to impite the missing valuees)


tempData <- mice(WDIVdem,m=5,maxit=1,meth='cart',seed=500)
summary(tempData)

WDIVdem <- complete(tempData,1)
####mice does not impute some missing, so We delete the variables with missing



WDIVdem <- WDIVdem %>% 
  select_if(~ !any(is.na(.)))

##creating splits------
set.seed(123)
WDIVdem_split <- initial_split(WDIVdem, prop=0.85,
                               strata=v2x_polyarchy, breaks = 4)
testing_WDIVdem <- testing(WDIVdem_split)
training_base_WDIVdem <- training(WDIVdem_split)
set.seed(345)
folds <- vfold_cv(training_base_WDIVdem, v = 10, strata = v2x_polyarchy, breaks = 4)
folds

###Creating the recipe: the predicted value is v2x
rec_WDI <- 
  recipe(v2x_polyarchy~ ., data = training_base_WDIVdem)

#####Random forest model---------


rforest_spec <- rand_forest(mtry=tune(), min_n = tune(), trees=1000) %>%
  set_mode ('regression') %>% 
  set_engine('ranger')
  
rforest_wf <- workflow() %>%
  add_model(rforest_spec) %>%
  add_recipe(rec_WDI)

extract_parameter_set_dials(rforest_spec)


tune_res <- tune_grid(
  rforest_wf,
  resamples = folds,
  grid = 20,
  options(parallel_over = "resamples")
)

tune_res

tune_res %>% 
  show_best(metric = "rmse")

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")



###We run the random forest for a number of predictors betwenn 110 and 200 and a number of leaves between 3 and 9 (which granted the best results for the first model)

rf_grid <- grid_regular(
  mtry(range = c(110,200)),
  min_n(range = c(3, 9)),
  levels = 5
)

rf_grid
set.seed(456)
regular_res <- tune_grid(
  rforest_wf,
  resamples = folds,
  grid = rf_grid,
  
)

regular_res %>% 
  show_best(metric = "rmse")


regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

####model seem to minimize error for 110 regressors and 3 leaves, but since error is increasing in N of regressors we test for a lower number 

rf_grid2 <- grid_regular(
  mtry(range = c(50, 110)),
  min_n (range = c(3,4)),
  levels = 3
)
set.seed(456)
regular_res2 <- tune_grid(
  rforest_wf,
  resamples = folds,
  grid = rf_grid2,
  
)

regular_res2 %>% 
  show_best(metric = "rmse")




regular_res2 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

#####as in the point before

rf_grid3 <- grid_regular(
  mtry(range = c(15, 50)),
  min_n (range = c(3,4)),
  levels = 3
)
set.seed(456)
regular_res3 <- tune_grid(
  rforest_wf,
  resamples = folds,
  grid = rf_grid3,
)

regular_res3 %>% 
  show_best(metric = "rmse")


regular_res3 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")
###regular_res3 RMSE is decreasing in N, so optimal n of regressors is around 50
###tiny differences, but seems the best shot



rf_best_params <- regular_res3 %>%
  tune::select_best("rmse")
rf_model_final <- rforest_spec %>% 
  finalize_model(rf_best_params)

rf_train_prediction <- rf_model_final %>%
  fit(
    formula = v2x_polyarchy ~ ., 
    data    = training_base_WDIVdem
  ) %>%
  predict(new_data = training_base_WDIVdem) %>%
  bind_cols(training(WDIVdem_split))
rf_score_train <- 
  rf_train_prediction %>%
  yardstick::metrics(v2x_polyarchy, .pred) %>%
  mutate(.estimate = format(round(.estimate, 10), big.mark = ","))

rf_test_prediction <- rf_model_final %>%
  fit(
    formula = v2x_polyarchy ~ ., 
    data    = testing_WDIVdem
  ) %>%
  predict(new_data = testing_WDIVdem) %>%
  bind_cols(testing(WDIVdem_split))


rf_score_test <- 
  rf_test_prediction %>%
  yardstick::metrics(v2x_polyarchy, .pred) %>%
  mutate(.estimate = format(round(.estimate, 10), big.mark = ","))


#VIPS, package to see the most important predictors: these are (list)


important_predictors <- rf_model_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(v2x_polyarchy ~ .,
      data = training_base_WDIVdem) %>%
  vip(geom = "point")







# Fit XGBoost

xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )


xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 30
  )
knitr::kable(head(xgboost_grid))

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(v2x_polyarchy ~ .)


xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE),
  options(parallel_over = "resamples")
)

xgboost_tuned %>% 
  show_best(metric = "rmse")

xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")
xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

xgb_train_prediction <- xgboost_model_final %>%
  fit(
    formula = v2x_polyarchy ~ ., 
    data    = training_base_WDIVdem
  ) %>%
  predict(new_data = training_base_WDIVdem) %>%
  bind_cols(training(WDIVdem_split))
xgboost_score_train <- 
  train_prediction %>%
  yardstick::metrics(v2x_polyarchy, .pred) %>%
  mutate(.estimate = format(round(.estimate, 10), big.mark = ","))


xgb_test_prediction <- xgboost_model_final %>%
  fit(
    formula = v2x_polyarchy ~ ., 
    data    = testing_WDIVdem
  ) %>%
  predict(new_data = testing_WDIVdem) %>%
  bind_cols(testing(WDIVdem_split))
xgboost_score_test <- 
  xgb_test_prediction %>%
  yardstick::metrics(v2x_polyarchy, .pred) %>%
  mutate(.estimate = format(round(.estimate, 10), big.mark = ","))



 





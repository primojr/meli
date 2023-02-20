#
# Ajuste do Randon Forest
library(tidymodels)

# 00 Split
split_df <- initial_split(df, strata = churn)
df_train <- training(split_df)
df_test <- testing(split_df)

df_train %>% janitor::tabyl(churn)
dim(df_train)
dim(df_test)

df$monthly_charges  %>% hist
df$monthly_charges %>% sqrt %>% hist
# Separar o monthly_charges


# 01 Pré Processamento
reg_recipe <- recipe(churn ~ ., data = df_train) %>%
  step_select(-gender,-phone_service,-customer_id) %>%
  step_mutate(
    contract = if_else(contract == 'Month-to-month', 'mensal', 'anual')
  ) %>%
  step_impute_mean(total_charges) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(tech_support, internet_service)

# 04. Engine
eng_RF  <-
  rand_forest(trees = 1000,mtry = tune(),min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")
# SVM
#install.packages("kernlab")
eng_svm <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")
#Logistico
eng_logistic <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# 05. Workflow
reg_workflow <- workflow_set(
  preproc = list(reg_recipe)
  ,models = list(eng_RF,eng_svm,eng_logistic)
) %>% mutate(wflow_id = gsub("recipe_","",wflow_id))

# 06.Cross validation
val_set <- vfold_cv(df_train, v = 5, strata = churn)
grade <- expand.grid(
  mtry = c(1, 2, 3, 5),
  min_n = 2^c(2, 4, 6)
)

grid_ctrl <- control_grid(
  save_pred = TRUE
  ,parallel_over = 'everything'
  ,save_workflow = TRUE
)

# 07.trainning
reg_trained <- reg_workflow %>%
  workflow_map(
    resamples = val_set
    ,grid = 10
    ,control = grid_ctrl
  )


# autoplot
ggplot2::autoplot(reg_trained)

# selecaop
reg_best_tune <- select_best(reg_trained, "accuracy")
final_reg_model <- reg_mod %>%
  finalize_model(reg_best_tune)

final_reg_model$eng_args

workflow() %>%
  add_recipe(reg_recipe) %>%
  add_model(final_reg_model) %>%
  collect_predictions() %>%
  select(.row, price, .pred) %>%
  ggplot() +
  aes(x= price, y = .pred) +
  geom_point()

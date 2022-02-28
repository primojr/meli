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
  step_select(-gender,-phone_service) %>%
  step_mutate(
    contract = if_else(contract == 'Month-to-month', 'mensal', 'anual')
    #,payment_method = payment_method %>% if_else(!str_detect(.,"automatic"), "automatic",.)
  ) %>%
  step_impute_knn(total_charges) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(tech_support)

# 04. Engine
reg_mod  <- rand_forest() %>%
  set_engine("ranger") %>%
  # set_args(k = 10, importance = TRUE) %>%
  set_mode("classification")

# 05. Workflow
reg_workflow <- workflow() %>%
  add_model(reg_mod) %>%
  add_recipe(reg_recipe)

# 06.Cross validation
val_set <- vfold_cv(df_train, v = 4, strata = churn)

# 07.trainning
reg_trained <- reg_workflow %>%
  tune_grid(
    val_set,
    grid = 5,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(accuracy)
  )

reg_trained %>% show_best()

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

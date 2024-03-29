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

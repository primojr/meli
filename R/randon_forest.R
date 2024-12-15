#
# Ajuste do Randon Forest
library(tidymodels)

# Ler os dados
df <- readr::read_csv("date/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>%
  janitor::clean_names()

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

# 02. Engine
eng_RF  <-
  rand_forest(trees = 1000,mtry = tune(),min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

eng_logistic <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# 03. Workflow
# Confirgurar o Workflow
reg_workflow <- workflow_set(
  preproc = list(reg_recipe)
  ,models = list(eng_RF,eng_logistic)
) %>% mutate(wflow_id = gsub("recipe_","",wflow_id))

# 04. Cross validation
val_set <- vfold_cv(df_train, v = 5, strata = churn)
# Grid de parametros
grade <- expand.grid(
  mtry = c(1, 2, 3, 5),
  min_n = 2^c(2, 4, 6)
)
# 05.Controle do treino
grid_ctrl <- control_grid(
  save_pred = TRUE
  ,parallel_over = 'everything'
  ,save_workflow = TRUE
)

# 06.trainning
reg_trained <- reg_workflow %>%
  workflow_map(
    resamples = val_set
    ,grid = 10
    ,control = grid_ctrl
  )


autoplot(reg_trained)

# Identificar o melhor algoritimo
autoplot(
  reg_trained
  ,rank_metric = 'accuracy'
  ,metrics = 'accuracy'
  ,select_best = TRUE
)

tune::show_best(reg_trained, "roc_auc")

# predicoes
wf <- reg_workflow %>% finalize_workflow(select_best(reg_trained, "roc_auc"))
ajuste_final <- last_fit(wf, splitis, metrics = metric_set(accuracy, roc_auc, f_meas, specificity, precision, recall))
predicoes_na_base_teste <- collect_predictions(ajuste_final)

## Graficos
# curva roc
roc <- predicoes_na_base_teste %>%
  roc_curve(churn, .pred_ComCartaoApp) %>%
  autoplot()

# curva de lift
lift <- predicoes_na_base_teste %>%
  lift_curve(classe1, .pred_ComCartaoApp) %>%
  autoplot()
# KS
ks <- predicoes_na_base_teste %>%
  ggplot(aes(x = .pred_ComCartaoApp, colour = classe1)) +
  stat_ecdf(show.legend = FALSE)

# distribuicao
dist <- predicoes_na_base_teste %>%
  ggplot(aes(x = .pred_ComCartaoApp, fill = classe1)) +
  geom_density() +
  theme(axis.title = element_blank())

install.packages("patchwork")



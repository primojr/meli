# Exemplo regressao logistica
# install.packages(c("pROC", "caret"))
# install.packages("janitor")

# Carregar pacotes
library(tidyverse)
library(tidymodels)
library(pROC)
library(ggplot2)
library(caret)
library(patchwork)

# Ler os dados
df <- readr::read_csv("date/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>%
  janitor::clean_names()

df %>% glimpse()
df %>% count(churn)

# Pré Processamento
df <- df %>%
  mutate(
    contract = if_else(contract == 'Month-to-month', 'mensal', 'anual'),
    tech_support = if_else(tech_support == 'No', 'No', 'Yes'),
    internet_service = if_else(internet_service == 'Fiber optic', 'Fiber optic', 'Other'),
    senior_citizen = as.factor(senior_citizen),
    total_charges = if_else(total_charges == ' ', NA_real_, as.numeric(total_charges)),
    total_charges = if_else(is.na(total_charges), mean(total_charges, na.rm = TRUE), total_charges),
    churn = as.factor(churn),
    ) %>%
  select(churn, contract, tenure, monthly_charges, total_charges, tech_support, internet_service)

# 00 Split
split_df <- initial_split(df, strata = churn)
df_train <- training(split_df)
df_test <- testing(split_df)

# Ajuste regressao logistica no treino
reg_model <- glm(churn ~ ., data = df_train, family = binomial)
summary(reg_model)

# Predição
df_test$pred <- predict(reg_model, newdata = df_test, type = 'response')
df_test %>% select(churn, pred) %>% sample_n(5)

# Curva ROC
roc_obj <- roc(df_test$churn, df_test$pred, levels = c("Yes", "No"))

# Extrair os valores da curva ROC
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# Grafico curva ROC
plot_roc <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "red", size = 1) +
  geom_abline(slope = 1, intercept = 0,linetype = "dashed", color = "gray") +
  labs(
    title = "Curva ROC",
    x = "1 - Especificidade (FPR)",
    y = "Sensibilidade (TPR)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico de densidade por classe
plot_desn <- ggplot(df_test, aes(x = pred, fill = churn)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Yes" = "red", "No" = "cyan")) +
  theme_minimal() +
  labs(title = "Distribuição de previsões por classe", x = "Predict", y = "Densidade")

# Curva acumulada para cada classe
plot_acum <- df_test %>%
  group_by(churn) %>%
  arrange(pred) %>%
  mutate(cumulativo = cumsum(pred) / sum(pred)) %>%
  ggplot(aes(x = pred, y = cumulativo, color = churn)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Yes" = "red", "No" = "cyan")) +
  theme_minimal() +
  labs(title = "Curva Acumulativa", x = "Pred", y = "y")

# Exibir os gráficos
plot_roc / (plot_desn + plot_acum)

# Matriz de confusão
df_test <- df_test %>%
  mutate(pred_class = if_else(pred > 0.5, 'Yes', 'No'))

# Criar a matriz de confusão
matriz_confusao <- confusionMatrix(
  as.factor(df_test$pred_class),
  as.factor(df_test$churn),
  positive = "Yes"
  )
# Exibir a matriz de confusão
print(matriz_confusao)

# Interpretacao

reg_model$coefficients

# Interpretacao
exp(reg_model$coefficients) %>% cbind






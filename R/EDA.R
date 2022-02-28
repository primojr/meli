## Carregar pacote
library(tidyverse)

# Ler os dados
df <- read_csv("date/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>%
  janitor::clean_names()

glimpse(df)

# Remover variaveis que não fazem parte do estudo
df <- df %>% select(-multiple_lines,-online_security, -payment_method,-online_backup,-device_protection, -streaming_tv,-streaming_movies)
df$churn = as.character(df$churn)

#EDA
# Entender os dados
skimr::skim(df)
df %>% DataExplorer::plot_histogram(ncol = 3, nrow = 2) # Continua
# Aplicar LOG e raiz

df[-1,] %>% DataExplorer::plot_bar(ncol = 5, nrow = 2) # Categorica
df %>% janitor::tabyl(churn)

# numerica pelo churn
df %>% DataExplorer::plot_boxplot(., by = "churn")

# Variveis Categoricas pelo churn
df[,-1] %>% DataExplorer::plot_bar(., by = "churn",ncol = 5, nrow = 3 )

df %>% select(where(is.numeric)) %>%
  na.omit() %>%
  DataExplorer::plot_correlation()


# Transformações:
# 1. Balancer amostra
# 2. Desconsiderar: phone_service, genero
# 3. Juntar contract 1-2
# 4. Metodo automatico unir


# Testar transaformações
df$total_charges %>% hist
df$total_charges %>% sqrt %>% hist

df$monthly_charges %>% hist
df$monthly_charges %>% log %>% hist

# Obs: Aplicando a raiz, apresenta uma certa unifomidade, demostrando ter 2 ou 3 padroes

# Aplicar arvore de descisão para testar importância das variaveis


# Transformar:
df <- df %>% mutate(
    contract = if_else(contract == 'Month-to-month','Mensal','Anual')
   ,monthly_charges = log(monthly_charges)
   ,monthly_charges = if_else(monthly_charges <= 3.5, 0,1)
   ,fibra = if_else(internet_service == 'Fiber optic',1,0)
)



# Arvore
df_tree <- df %>% select(-customer_id,-phone_service, -gender,-internet_service,-total_charges)

fit <- rpart::rpart(formula = churn ~ .
                    ,data = df_tree
                    ,method = 'class'
                    ,parms = list(split = "gini")
                    ,cp = 0.00002)

fit$variable.importance %>%
  barplot( las=1
           ,cex.axis =  .8
           ,cex.names = .6
           ,offset = -5
  )

#
poda <- rpart::prune.rpart(fit, cp = .003)
rpart.plot::rpart.plot(poda
                       ,type = 0
                       ,extra = 101
                       ,box.palette = "GnBu"
                       ,branch.lty=2
                       ,shadow.col = 'gray'
                       ,nn = TRUE
                       ,cex = .8
)




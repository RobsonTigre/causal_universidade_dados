##########################################################
## Curso: Inferência Causal
## Instrutor: Robson Tigre
## Criação: 01.11.24
## Última modificação: 04.11.24
##########################################################

# Setup -------------------------------------------------------------------
# install.packages("rdrobust")
# install.packages("rddensity")
# install.packages("lpdensity")
library(ggplot2)
library(rdrobust)
library(rddensity)
library(lpdensity)
library(dplyr)

rm(list = ls())


# Data generation ---------------------------------------------------------

# EN-US: Set seed for reproducibility
# PT-BR: Defina a semente para reprodutibilidade
set.seed(1213456)

# EN-US: Define the total number of observations in this analysis
# PT-BR: Defina o número total de observações nesta análise
n <- 20000

# EN-US: Simulate the running variable 'x' (e.g., loyalty score in the recent past) - in this case, it's normally distributed with mean 50
# PT-BR: Simule a running variable 'x' (ex: pontuação de loyalty do usuário no passado recente) - neste caso, é distribuída normalmente com média 50
x <- rnorm(n, mean = 50, sd = 10) # Engagement score centered around 50


# EN-US: Simulate covariates: Age, user tenure, and purchase frequency
# PT-BR: Simule covariáveis: Idade, tempo como usuário e frequência de compras
age <- rnorm(n, mean = 35, sd = 5)  # Users are around 35 years old on average
tenure <- rnorm(n, mean = 12, sd = 3) # Users have around 12 months tenure on average
purchase_freq <- rpois(n, lambda = 5) # Poisson distribution for frequency of monthly purchases

# EN-US: Simulate the outcome variable 'y' (e.g., future total spending, TGMV) with a sharp jump of R$200 at x = 50
# PT-BR: Simule a variável de resultado 'y' (ex: gasto futuro total, TGMV) com um salto de R$200 em x = 50
y <- ifelse(x >= 50, 1000 + 10 * x + 250, 1000 + 10 * x) + rnorm(n, 0, 200)

# EN-US: Gather everything in a data frame
# PT-BR: Reunir tudo em um data frame
data <- data.frame(x = x, y = y, age = age, tenure = tenure, purchase_freq = purchase_freq)


# Descriptive visualization for the whole population ----------------------

# EN-US: Plot the descriptive data to visualize the discontinuity with two regression lines
# PT-BR: Plotar os dados descritivos para visualizar a descontinuidade com duas linhas de regressão
ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
  # Regression line for x <= 50
  geom_smooth(data = data[data$x <= 50, ], method = "lm", se = FALSE, color = "#e3275f") +
  # Regression line for x > 50
  geom_smooth(data = data[data$x > 50, ], method = "lm", se = FALSE, color = "#2772e3") +
  labs(title = "Simulated regression discontinuity",
       x = "x = loyalty score in the recent past", 
       y = "y = future total spending in R$") +
  theme_minimal()

# RDD checks and results --------------------------------------------------

# EN-US: The "no manipulation" check: density check of running variable around the cutoff
# PT-BR: O teste de "não manipulação": verificação da densidade da running variable em torno do cutoff
density_test <- rddensity(data$x, c = 50)
summary(density_test)
rdplotdensity(density_test, data$x, 
              title = "Density plotting for manipulation testing",
              xlabel = "Loyalty score in the recent past",
              ylabel = "Density",) # no significant discontinuity

# EN-US: Testing the continuity of covariates (covariates should be balanced - i.e., shouldn't jump - across the threshold)
# PT-BR: Testando a continuidade das covariáveis (as covariáveis devem ser balanceadas - não devem ter saltos - ao redor do threshold)
covariate_age <- rdrobust(data$age, data$x, c = 50)
summary(covariate_age) # no discontinuity in age -> p-value "Robust" > 0.1 -> good!

covariate_tenure <- rdrobust(data$tenure, data$x, c = 50)
summary(covariate_tenure) # no discontinuity in tenure -> p-value "Robust" > 0.1 -> good!

covariate_freq <- rdrobust(data$purchase_freq, data$x, c = 50)
summary(covariate_freq) # no discontinuity in freq -> p-value "Robust" > 0.1 -> good!

# EN-US: Perform RDD analysis on the outcome y using rdrobust package
# PT-BR: Realize análise RDD no outcome y, também usando o pacote rdrobust
rdd_result <- rdrobust(data$y, data$x, c = 50)
summary(rdd_result) # no discontinuity in y = Purchases -> p-value "Robust" < 0.05  -> good, significant increase in y!

# EN-US: Robustness exercise: including covariables in the estimation
# PT-BR: Exercício de robustez: incluindo covariáveis na estimação
rdd_result_cov <- rdrobust(data$y, data$x, c = 50, covs = data[, c("age", "tenure", "purchase_freq")])
summary(rdd_result_cov)


# EN-US: Finally, a Formal visualization RDD with rdplot, with proper binning out Y to avoid pollution, and confidence intervals
# PT-BR: Finalmente, uma visualização formal do RDD com rdplot, com binning adequado de Y para evitar poluição, e intervalos de confiança
subset_data <- data %>% filter(x > 40 & x < 60) # Subset data where x is between 40 and 60
rdplot(y = subset_data$y, x = subset_data$x, c = 50, ci = 95, # RDD Plot with the subset data and confidence intervals
       title = "Regression discontinuity plot",
       y.label = "y = future total spending in R$", 
       x.label = "x = loyalty score in the recent past")

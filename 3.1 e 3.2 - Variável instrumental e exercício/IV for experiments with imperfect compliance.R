##########################################################
## Curso: Inferência Causal
## Instrutor: Robson Tigre
## Criação: 05.10.24
## Última modificação: 05.10.24
##########################################################

# Setup -------------------------------------------------------------------
required_packages <- c("fixest", "AER", "tidyverse", "janitor")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(AER)
library(fixest)
library(tidyverse)
library(janitor)

rm(list = ls())
options(scipen = 999)


# Data generation ---------------------------------------------------------

# EN-US: Set seed for reproducibility
# PT-BR: Defina a semente para reprodutibilidade
set.seed(123)


# EN-US: Define the total number of observations in this analysis
# PT-BR: Defina o número total de observações nesta análise
n <- 20000


# EN-US: Random assignment of clients to treatment: 50% assigned to receive a coupon (treatment) and 50% to not receive it (control group)
# PT-BR: Atribuição aleatória de clientes ao tratamento: 50% atribuídos para receber um cupom (tratamento) e 50% para não receber (controle)
treatment <- rbinom(n, 1, 0.5)


# EN-US: To be more realistic, simulate covariates that are correlated with future_purchases (dependent var.) and coupon_use (consumption of the treatment)
# PT-BR: Para ser mais realista, simule covariáveis que são correlacionadas com future_purchases (variável dependente) e coupon_use (uso do tratamento)
income <- rnorm(n, mean = 50000, sd = 15000)
age <- rnorm(n, mean = 35, sd = 10)


# EN-US: Now make compliance (i.e., using the coupon upon receiveing it) dependent on the covariates... 
# ... here, higher income and younger age increase the likelihood of using the coupon
# PT-BR: Agora, torne o compliance (i.e., usar o cupom ao recebê-lo) dependente das covariáveis... 
# ... aqui, renda mais alta e ser mais jovem aumentam a probabilidade de usar o cupom
prob_coupon_use <- ifelse(treatment == 1,
                          plogis(-2 + 0.00008 * income - 0.08 * age),  # Logistic function
                          0)  # No coupon use in control group


# EN-US: Generate coupon usage (1 = yes, 0 = no) given prob_coupon_use...
# ... rbinom(n = number of observations, 1 = number of trials, prob_coupon_use = probability of success on each trial)
# PT-BR: Gerar uso de cupom (1 = sim, 0 = não) dado prob_coupon_use...
# ... rbinom(n = número de observações, 1 = número de tentativas, prob_coupon_use = probabilidade de sucesso em cada tentativa)
coupon_use <- rbinom(n, 1, prob_coupon_use) 


# EN-US: Simulate the outcome, which is also a function of income and age. Future purchases are higher for treated clients who used the coupon
# PT-BR: Simular o outcome, que também é uma função da renda e da idade. As compras futuras são maiores para clientes tratados que usaram o cupom
past_purchases <- rnorm(n, mean = 100, sd = 30) + 0.001 * income + 0.5 * age
baseline_purchases <- rnorm(n, mean = 100, sd = 20) + 0.001 * income + 0.5 * age
true_effect <- 50  # Coupon usage increases future purchases by $50
future_purchases <- baseline_purchases + true_effect * coupon_use # Generate observed future purchases


# EN-US: Gather everything in a data frame
# PT-BR: Reunir tudo em um data frame
data <- data.frame(
  treatment = treatment,
  coupon_use = coupon_use,
  past_purchases = past_purchases,
  future_purchases = future_purchases,
  income = income,
  age = age
)

data <- data %>%
  mutate(group = case_when(
    treatment == 0 ~ "Control",
    treatment == 1 & coupon_use == 0 ~ "Non-compliers",
    treatment == 1 & coupon_use == 1 ~ "Compliers"))

data %>% 
  filter(treatment == 1) %>% 
  janitor::tabyl(coupon_use) # see compliance rate


# Check IV assumptions ----------------------------------------------------

# EN-US: Assumption 1 - Relevance: Coupon assignment must significantly affect coupon usage
# PT-BR: Pressuposto 1 - Relevância: A atribuição de cupons deve afetar significativamente o uso dos cupons
summary(lm(coupon_use ~ treatment + income + age, data = data))

# EN-US: Assumption 2 - Independence/exclusion restriction: indirect test -> regressions with or without covariates should yield similar results ...
# .. to support the idea that our instrument (coupon assignment) affects the outcome (future purchases) only through the endogenous variable (coupon use).
# PT-BR: Pressuposto 2 - Independência/Restrição de Exclusão: teste indireto -> regressões com ou sem covariáveis devem produzir resultados semelhantes ...
# ... para sustentar a ideia de que nosso instrumento (atribuição de cupom) afeta o outcome (compras futuras) apenas através da variável endógena (uso do cupom),
summary(lm(coupon_use ~ treatment, data = data)) # 1st stage without covariates, just to be sure
summary(ivreg(future_purchases ~ coupon_use | treatment, data = data)) # without covariates
summary(ivreg(future_purchases ~ coupon_use + income + age | treatment + income + age, data = data)) # with covariates


#  EN-US: Conducting a placebo test to test whether the instrument affects past purchases (which it  logically shouldn't)...
# ... This indicates that the instrument does not have a direct effect on the outcome outside of its effect through the endogenous variable.
# PT-BR: Realizando um teste placebo para testar se o instrumento afeta compras passadas (o que logicamente não deveria)...
# ... Isso indica que o instrumento não tem um efeito direto no desfecho fora de seu efeito através da variável endógena.
summary(ivreg(past_purchases ~ coupon_use + income + age | treatment + income + age, data = data))


# Results: ITT and LATE ---------------------------------------------------

# EN-US: Estimate the Intention-to-Treat (ITT) effect
# PT-BR: Estimar o Efeito de Intenção de Tratar (ITT)
summary(lm(future_purchases ~ treatment + income + age, data = data))

# EN-US: Estimate the Local Average Treatment Effect (LATE) using IV
# PT-BR: Estimar o Efeito Médio Local de Tratamento (LATE) usando VI
summary(ivreg(future_purchases ~ coupon_use + income + age | treatment + income + age, data = data)) # Using ivreg from the AER package
summary(feols(future_purchases ~  income + age | coupon_use ~ treatment, data = data)) # Using feols from the fixest package


# Visualization -----------------------------------------------------------

mean_control <- mean(data$future_purchases[data$treatment == 0]) # Mean of future purchases for the control group
mean_non_compliers <- mean(data$future_purchases[data$treatment == 1 & data$coupon_use == 0]) # Mean of future purchases for the non-compliers
mean_compliers <- mean(data$future_purchases[data$treatment == 1 & data$coupon_use == 1]) # # Mean of future purchases for the compliers

mean_compliers - mean_non_compliers # difference in means between compliers and non-compliers
mean_compliers - mean_control # difference in means between compliers and control

# EN-US: Plot the distributions of control, non-compliers, and compliers
# PT-BR: Plotar as distribuições dos grupos Controle, Não-compliers e compliers
p1 <- ggplot(data, aes(x = future_purchases, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean_control - 5, color = "Control"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_non_compliers + 5, color = "Non-compliers"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_compliers, color = "Compliers"), linetype = "dashed", size = 1) +
  geom_text(aes(x = mean_control - 15, label = round(mean_control, 2), y = 0.0155), color = "black", angle = 0, vjust = -1, size = 6) +
  geom_text(aes(x = mean_non_compliers + 15, label = round(mean_non_compliers, 2), y = 0.0155), color = "black", angle = 0, vjust = -1, size = 6) +
  geom_text(aes(x = mean_compliers + 10, label = round(mean_compliers, 2), y = 0.015), color = "black", angle = 0, vjust = -1, size = 6) +
  labs(title = "Distribution of outcome variable future_purchases by group",
       x = "Outcome (future_purchases)",
       y = "Density") +
  scale_fill_manual(values = c("Control" = "red", "Non-compliers" = "#23ae4d", "Compliers" = "blue")) +
  scale_color_manual(values = c("Control" = "red", "Non-compliers" = "#23ae4d", "Compliers" = "blue")) +
  theme_minimal(base_size = 18) +  # Increase all text sizes
  theme(legend.position = "bottom") # Position legend below the plot

print(p1)


# EN-US: Create a combined distribution for the treated group (compliers + non-compliers)
# PT-BR: Criar uma distribuição combinada para o grupo tratado (compliers + não-compliers)
data_combined <- data %>%
  mutate(group = ifelse(treatment == 1, "Treated (compliers [36%] + non-compliers [64%])", "Control"))

# Calculate the mean for the treated group
mean_treated <-  mean(data$future_purchases[data$treatment == 1]) # Mean of future purchases for the the treated

# EN-US: Plot the combined distribution of the treated group vs control group
# PT-BR: Plotar a distribuição combinada do grupo tratado vs grupo de controle
p2 <- ggplot(data_combined, aes(x = future_purchases, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean_control, color = "Control"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean_treated, color = "Treated (compliers [36%] + non-compliers [64%])"), linetype = "dashed", size = 1) +
  geom_text(aes(x = mean_control - 5, label = round(mean_control, 2), y = 0.016), color = "black", angle = 0, vjust = -1, size = 6) +
  geom_text(aes(x = mean_treated + 5, label = round(mean_treated, 2), y = 0.016), color = "black", angle = 0, vjust = -1, size = 6) +
  labs(title = "Combined distribution of treated (compliers + non-compliers) vs control",
       x = "Outcome (future_purchases)",
       y = "Density") +
  scale_fill_manual(values = c("Control" = "red", 
                               "Treated (compliers [36%] + non-compliers [64%])" = "#23aeFF")) +
  scale_color_manual(values = c("Control" = "red", 
                                "Treated (compliers [36%] + non-compliers [64%])" = "#23aeFF")) +
  theme_minimal(base_size = 18) +  # Increase all text sizes
  theme(legend.position = "bottom") # Position legend below the plot

print(p2)

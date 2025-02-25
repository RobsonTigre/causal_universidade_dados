##########################################################
## Curso: Inferência Causal
## Instrutor: Robson Tigre
## Criação: 01.11.24
## Última modificação: 01.11.24
##########################################################

# # Limpa ambiente
rm(list = ls())
options(scipen=999)

# Carreganto pacote necessário
if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if(!require(gridExtra)) install.packages("gridExtra", dependencies=TRUE)

# Seed para replicabilidade
set.seed(123)

# Parâmetros para ambos cenários
mean_diff <- 20 # Diferença de média entre tratamento e controle
n <- 10000 # 10.000 observações

# Exemplo com alta variância
mean1_high_var <- 100
mean2_high_var <- mean1_high_var + mean_diff
sd_high_var <- 20

# Gerando dados para o cenário com alta variância
group1_high_var <- rnorm(n, mean = mean1_high_var, sd = sd_high_var)
group2_high_var <- rnorm(n, mean = mean2_high_var, sd = sd_high_var)

# Exemplo com baixa variância
mean1_low_var <- 100
mean2_low_var <- mean1_low_var + mean_diff
sd_low_var <- 5

# Gerando dados para o cenário com baixa variância
group1_low_var <- rnorm(n, mean = mean1_low_var, sd = sd_low_var)
group2_low_var <- rnorm(n, mean = mean2_low_var, sd = sd_low_var)

# Combinando dados em um dataframe
data_high_var <- data.frame(
  value = c(group1_high_var, group2_high_var),
  group = factor(rep(c("Controle", "Tratamento"), each = n))
)

data_low_var <- data.frame(
  value = c(group1_low_var, group2_low_var),
  group = factor(rep(c("Controle", "Tratamento"), each = n))
)

# Plot Exemplo com alta variância
p1 <- ggplot(data_high_var, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean1_high_var), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = mean2_high_var), linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 0.04)) +
  scale_x_continuous(limits = c(20, 200)) +
  annotate("text", x = mean1_high_var-10, y = 0.025, label = paste("Média controle =", mean1_high_var), color = "blue", angle = 0, vjust = -0.5, size = 6) +
  annotate("text", x = mean2_high_var+10, y = 0.02, label = paste("Média tratamento =", mean2_high_var), color = "red", angle = 0, vjust = -0.5, size = 6) +
  labs(title = "Exemplo com alta variância",
       x = "Valor da variável dependente y",
       y = "Densidade",
       fill = "Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# Plot Low Variance Example
p2 <- ggplot(data_low_var, aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean1_low_var), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = mean2_low_var), linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 0.085)) +
  scale_x_continuous(limits = c(60, 150)) +
  annotate("text", x = mean1_low_var, y = 0.082, label = paste("Média controle =", mean1_low_var), color = "blue", angle = 0, vjust = -0.5, size = 6) +
  annotate("text", x = mean2_low_var, y = 0.076, label = paste("Média tratamento =", mean2_low_var), color = "red", angle = 0, vjust = -0.5, size = 6) +
  labs(title = "Exemplo com baixa variância",
       x = "Valor da variável dependente y",
       y = "Densidade",
       fill = "Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


# Mostrar os plots lado a lado
grid.arrange(p1, p2, ncol = 2)

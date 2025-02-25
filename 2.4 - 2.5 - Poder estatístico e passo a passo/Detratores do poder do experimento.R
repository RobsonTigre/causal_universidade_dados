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
if(!require(tidyr)) install.packages("tidyr", dependencies=TRUE)
if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)


# Função para simular dados e calcular poder
simulate_power <- function(n_total, sd_compras, prop_treatment, 
                           true_effect_size_percent = 5, n_simulations = 1000,
                           significance_level = 0.05) {
  
  base_value <- 100
  true_effect_size <- base_value * (true_effect_size_percent/100)
  significant_tests <- 0
  
  for(i in 1:n_simulations) {
    # Gera dados do grupo de controle
    n_control <- round(n_total * (1 - prop_treatment))
    control_data <- rnorm(n_control, mean = base_value, sd = sd_compras)
    
    # Gera grupo dados do grupo de tratamento com efeito de 5%
    n_treatment <- n_total - n_control
    treatment_data <- rnorm(n_treatment, mean = base_value + true_effect_size, sd = sd_compras)
    
    # Junta tudo em um dataframe
    data <- data.frame(
      compras = c(control_data, treatment_data),
      recebeu_email = c(rep(0, n_control), rep(1, n_treatment))
    )
    
    # Executa t-test
    test_result <- t.test(compras ~ recebeu_email, data = data)
    
    # Conta resultados estatisticamente significantes
    if(test_result$p.value < significance_level) {
      significant_tests <- significant_tests + 1
    }
  }
  
  return(significant_tests / n_simulations)
}

# Define o tema para todos os plots

my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.margin = margin(20, 20, 20, 20)  # Add more margin around plots
  )

#######################################
# 1. Efeito do tamanho da amostra sobre poder do experimento
#######################################
sample_sizes <- seq(100, 1000, by = 50)  # Increased range
power_by_size <- sapply(sample_sizes, function(n) {
  simulate_power(n, sd_compras = 20, prop_treatment = 0.5)
})

# Econtra onde o poder cruza a linha de 80%
power_df <- data.frame(sample_size = sample_sizes, power = power_by_size)
min_n_for_power <- min(power_df$sample_size[power_df$power >= 0.8])

size_plot <- ggplot(power_df, aes(x = sample_size, y = power)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = min_n_for_power, linetype = "dashed", color = "red") +
  annotate("text", x = min_n_for_power, y = 0.4, 
           label = paste("Min N =", min_n_for_power), 
           angle = 90, vjust = -0.5, size = 5) +
  annotate("text", x = max(sample_sizes), y = 0.82, 
           label = "Poder de 80%", hjust = 1, size = 5) +
  labs(title = "Efeito do tamanho da amostra (N) sobre poder",
       x = "Tamanho da amostra",
       y = "Poder",
       subtitle = "α = 0.05, Effect size de 5%, SD = 20, Proporção grupo de tratamento = 0.5") +
  my_theme +
  ylim(0, 1)

#######################################
# 2. Efeito da variância do y sobre poder
#######################################
sds <- seq(15, 50, by = 5)
power_by_sd <- sapply(sds, function(sd) {
  simulate_power(n_total = 800, sd_compras = sd, prop_treatment = 0.5)  # Increased N
})

variance_plot <- ggplot(data.frame(sd = sds, power = power_by_sd),
                        aes(x = sd, y = power)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  annotate("text", x = max(sds), y = 0.82, 
           label = "Poder de 80%", hjust = 1, size = 5) +
  labs(title = "Efeito do desvio padrão sobre o poder estatístico",
       x = "Desvio padrão",
       y = "Poder",
       subtitle = "α = 0.05, Effect size de 5%, N = 800, Proporção grupo de tratamento = 0.5") +
  my_theme +
  ylim(0, 1)

#######################################
# 3. Efeito da proporção de alocados ao grupo de tratamento sobre poder estatístico
#######################################
props <- seq(0.1, 0.9, by = 0.1)
power_by_prop <- sapply(props, function(prop) {
  simulate_power(n_total = 800, sd_compras = 20, prop_treatment = prop)  # Increased N
})

prop_plot <- ggplot(data.frame(proportion = props, power = power_by_prop),
                    aes(x = proportion, y = power)) +
  geom_line(color = "green4", size = 1.2) +
  geom_point(color = "green4", size = 2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 0.5, y = 0.7,
           label = "Proporção ótima (50/50)", angle = 90, vjust = -0.5, size = 4.5) +
  annotate("text", x = 0.85, y = 0.82, 
           label = "Poder de 80%", hjust = 1, size = 4.5) +
  labs(title = "Efeito da Alocação dos Grupos no Poder Estatístico",
       x = "Proporção no Grupo de Tratamento",
       y = "Poder",
       subtitle = "α = 0.05, 5% Effect Size, N = 800, SD = 20") +
  my_theme +
  scale_y_continuous(breaks = seq(0.3, 1, by = 0.1))  # Added to create nice axis breaks

# Display plots
par(mfrow = c(2, 2))
print(size_plot)
print(variance_plot)
print(prop_plot)

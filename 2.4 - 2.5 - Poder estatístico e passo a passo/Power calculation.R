##########################################################
## Curso: Inferência Causal
## Instrutor: Robson Tigre
## Criação: 01.11.24
## Última modificação: 04.11.24
##########################################################

# Limpa ambiente
rm(list = ls())

# Instala pacotes necessários e carrega pacotes necessários
if (!require("pwr")) install.packages("pwr")
if (!require("effectsize")) install.packages("effectsize")


# 1. Exemplo inicial para um e-commerce -----------------------------------

# Exemplo 1: Teste A/B para impacto de um novo sistema de recomendação sobre valor das compras
# Dados históricos:
# - Média de compra do grupo de controle: R$ 150
# - Aumento esperado: R$ 15 (10%)
# - Desvio padrão combinado: R$ 45
# - Consira que 50% dos clientes serão alocados para cada grupo

# Escrevi essa função simples para retornar o efeito do Cohen's d
calculate_cohens_d <- function(mean1, mean2, sd_pooled) {
  d <- abs(mean1 - mean2) / sd_pooled
  return(d)
}

# Calculando Cohen's d para esse exemplo
purchase_effect_size <- calculate_cohens_d(
  mean1 = 150,
  mean2 = 165,
  sd_pooled = 45
)

# Função que traduz o Cohen's d à tabela de Cohen do slide 13
interpret_cohens_d(purchase_effect_size, rules = "cohen1988")

# Calcula tamanho de amostra necessário
purchase_test <- pwr.t.test(
  d = purchase_effect_size,
  power = 0.80,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

# Plota curva de poder
plot(
  purchase_test,
  main = "Análise de poder para o teste de valor das compras",
  xlab = "Tamanho da amostra (por grupo)",
  ylab = "Poder"
)



# 2. Análise de poder com diferentes tamanhos de MDE ----------------------

# Calcula tamanhos de amostra para diferentes tamanhos de efeito
effect_sizes <- seq(0.1, 0.5, 0.1)
sample_sizes <- sapply(effect_sizes, function(d) {
  ceiling(pwr.t.test(
    d = d,
    power = 0.80,
    sig.level = 0.05,
    type = "two.sample"
  )$n)
})

# Plota relação entre tamanho do efeito e tamanho da amostra
plot(
  effect_sizes,
  sample_sizes,
  type = "b",
  xlab = "Diferentes tamanhos de efeito (Cohen's d)",
  ylab = "Tamanho necessário de cada grupo",
  main = "Tamanho necessário de cada grupo por effect size",
  pch = 19,
  xaxp = c(0.1, 0.5, 8),    # Início, fim, número de intervalos para eixo x
  yaxp = c(0, max(sample_sizes), 10)  # Início, fim, número de intervalos para eixo x
)


# 3. Análise de poder com diferentes parâmetros ---------------------------

# Cria análise de poder para diferentes níveis de significância
alpha_levels <- c(0.01, 0.05, 0.10)
n_seq <- seq(10, 200, length.out = 100)

# Calcula poder para diferentes tamanhos de amostra e níveis alfa
power_matrix <- sapply(alpha_levels, function(alpha) {
  sapply(n_seq, function(n) {
    pwr.t.test(
      n = n,
      d = 0.3,   # Tamanho de efeito moderado
      sig.level = alpha,
      type = "two.sample"
    )$power
  })
})

# Plota curvas de poder para diferentes níveis de significância
matplot(
  n_seq,
  power_matrix,
  type = "l",
  lty = 1:3,
  lwd = 2,
  col = c("red", "blue", "green"),
  xlab = "Tamanho da amostra por grupo",
  ylab = "Poder",
  main = "Curvas de poder para diferentes valores de α (significância)"
)
legend(
  "bottomright",
  legend = paste("α =", alpha_levels),
  col = c("red", "blue", "green"),
  lty = 1:3
)


# 4. Função para exemplo prático ------------------------------------------

ecommerce_power_analysis <- function(
    current_value,
    expected_increase_pct,
    sd_estimate,
    desired_power = 0.80,
    alpha = 0.05
) {
  # Calcula médias
  mean1 <- current_value
  mean2 <- current_value * (1 + expected_increase_pct/100)
  
  # Calcula tamanho do efeito
  effect_size <- calculate_cohens_d(mean1, mean2, sd_estimate)
  
  # Realiza análise de poder
  result <- pwr.t.test(
    d = effect_size,
    power = desired_power,
    sig.level = alpha,
    type = "two.sample"
  )
  
  # Cria resumo
  summary <- list(
    current_value = current_value,
    expected_new_value = mean2,
    absolute_difference = mean2 - mean1,
    effect_size = effect_size,
    required_sample_size = ceiling(result$n),
    power = result$power,
    alpha = alpha
  )
  
  return(summary)
}

# Exemplo de uso:
example_analysis <- ecommerce_power_analysis(
  current_value = 150,    # Valor médio atual de compra
  expected_increase_pct = 10,  # Aumento esperado de 10%
  sd_estimate = 45,      # Desvio padrão estimado
  desired_power = 0.80,
  alpha = 0.05
)

print(example_analysis) # note que o sample size aqui é o número EM CADA GRUPO


# 5. Análise de poder com proporções diferentes de 50%/50% ----------------

# Função para analisar poder com alocação desigual
analyze_unequal_allocation <- function(
    effect_size,
    total_sample_size,
    allocation_ratios = seq(0.05, 0.95, by = 0.1),
    alpha = 0.05
) {
  # Calcula poder para diferentes proporções de alocação
  powers <- sapply(allocation_ratios, function(ratio) {
    n1 <- total_sample_size * ratio        # tamanho do grupo de tratamento
    n2 <- total_sample_size * (1 - ratio)  # tamanho do grupo de controle
    
    pwr.t2n.test(
      n1 = n1,
      n2 = n2,
      d = effect_size,
      sig.level = alpha
    )$power
  })
  
  return(data.frame(
    ratio = allocation_ratios,
    power = powers
  ))
}

# Exemplo de uso com cenário realista de e-commerce
total_n <- 1000  # Tamanho total da amostra
effect_size <- 0.3  # Tamanho de efeito moderado

# Calcula poder para diferentes proporções de alocação
allocation_analysis <- analyze_unequal_allocation(
  effect_size = effect_size,
  total_sample_size = total_n
)

# Plota relação entre proporção de alocação e poder
plot(
  allocation_analysis$ratio,
  allocation_analysis$power,
  type = "b",
  xlab = "Proporção de clientes alocados para o tratamento",
  ylab = "Poder estatístico",
  main = "Poder vs proporção de alocação",
  pch = 19
)
abline(v = 0.5, lty = 2, col = "red")


plot(
  allocation_analysis$ratio,
  allocation_analysis$power,
  type = "b",
  xlab = "Proporção de clientes alocados para o tratamento",
  ylab = "Poder estatístico",
  main = "Poder vs proporção de alocação",
  pch = 19,
  xaxp = c(0.05, 0.95, 5),  # Começa em 0.1, termina em 0.9, 16 intervalos
  yaxp = c(0.5, 1, 10)  # 10 intervalos para poder
)
abline(v = 0.5, lty = 2, col = "red")  # Adiciona linha de referência para alocação igual


# 6. Análise de alocação ajustada por custo de aplicar o tratamento -------

analyze_allocation_with_costs <- function(
    effect_size,
    total_budget,
    control_cost = 1,
    treatment_cost = 2,  # Treatment typically costs more
    allocation_ratios = seq(0.1, 0.9, by = 0.1),
    alpha = 0.05
) {
  # Calcula tamanhos de amostra e poder para diferentes proporções de alocação
  results <- sapply(allocation_ratios, function(ratio) {
    # Calcula tamanhos de amostra baseados na restrição orçamentária
    # Orçamento = n1*custo_tratamento + n2*custo_controle
    # Onde n1 = total_n * ratio e n2 = total_n * (1-ratio)
    total_n <- total_budget / (treatment_cost * ratio + control_cost * (1 - ratio))
    
    n1 <- floor(total_n * ratio)        # tamanho do grupo de tratamento
    n2 <- floor(total_n * (1 - ratio))  # tamanho do grupo de controle
    
    # Calcula poder
    power <- pwr.t2n.test(
      n1 = n1,
      n2 = n2,
      d = effect_size,
      sig.level = alpha
    )$power
    
    return(c(n1 = n1, n2 = n2, power = power))
  })
  
  return(data.frame(
    ratio = allocation_ratios,
    treatment_n = results[1,],
    control_n = results[2,],
    power = results[3,],
    total_n = results[1,] + results[2,]
  ))
}

# Exemplo com considerações de custo
budget_analysis <- analyze_allocation_with_costs(
  effect_size = 0.3,
  total_budget = 10000,
  control_cost = 1,
  treatment_cost = 2
)

# Ajusta as margens do plot para acomodar os dois gráficos
par(mar = c(4, 4, 3, 2))  # Ajusta margens (bottom, left, top, right)
par(mfrow = c(2, 1))      # Define layout 2x1
par(oma = c(0, 0, 2, 0))  # Ajusta margens externas

# Gráfico de poder
plot(
  budget_analysis$ratio,
  budget_analysis$power,
  type = "b",
  xlab = "Proporção os clientes alocada para tratamento",
  ylab = "Poder estatístico",
  main = "Poder vs alocação do tratamento (ajustado por custo)",
  pch = 19,
  cex.main = 0.9,    
  cex.lab = 0.8,     
  cex.axis = 0.8     
)
abline(v = 0.5, lty = 2, col = "red")

# Gráfico de tamanho da amostra
plot(
  budget_analysis$ratio,
  budget_analysis$total_n,
  type = "b",
  xlab = "Proporção Alocada para Tratamento",
  ylab = "Tamanho total da amostra",
  main = "Tamanho total da amostra vs alocação para tratamento",
  pch = 19,
  cex.main = 0.9,    
  cex.lab = 0.8,     
  cex.axis = 0.8     
)

# Restaura configurações originais
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # Restaura margens padrão


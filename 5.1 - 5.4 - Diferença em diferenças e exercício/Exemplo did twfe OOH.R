##########################################################
## Curso: Inferência Causal
## Instrutor: Robson Tigre
## Criação: 25.01.25
## Última modificação: 25.01.25
##########################################################


library(tidyverse)
library(fixest)
library(plm)

rm(list = ls())
gc()

# Simulação de dados
set.seed(789)
n_lojas <- 100   # Número de lojas
n_periodos <- 10 # Número de períodos

# Criando painel de dados balanceado, com efeito fixo de loja e correlacao serial
dados <- expand_grid(
  loja = 1:n_lojas,             # IDs das lojas
  periodo = 1:n_periodos) %>%   # Períodos de análise
  mutate(
    estado = ifelse(loja <= 0.6 * n_lojas, "PE", "PB"), # Estados: 60% em Pernambuco (controle) e 40% em Paraíba (tratamento)
    tratamento = ifelse(estado == "PB", 1, 0), # Define o grupo tratado e o período do tratamento
    efeito_loja = rnorm(n_lojas, mean = 2, sd = 1)[loja]) %>%  # Criando efeito fixo por cada loja
    # Agrupar por loja para simular autocorrelação
    group_by(loja) %>%  
      # Simular erros com autocorrelação AR(1) para cada loja
      mutate(error = arima.sim(model = list(ar = 0.6),  # Coeficiente de autocorrelação (ajuste conforme necessário)
          n = n_periodos,
          innov = rnorm(n_periodos, sd = 2))) %>% # Inovações aleatórias
    ungroup() %>%
    mutate(vendas_base = ((1 + (periodo/20)) * rnorm(n(), mean = ifelse(estado == "PE", 12, 10), sd = 2)) + efeito_loja + error,  # Vendas baseline simuladas em milhões de reais e ajustadas pelo efeito fixo
    # Aplicação do efeito do tratamento com crescimento moderado e estabilização
    vendas = case_when(tratamento == 1 & periodo == 7 ~ vendas_base * 1.10,
                       tratamento == 1 & periodo == 8 ~ vendas_base * 1.25,
                       tratamento == 1 & periodo == 9 ~ vendas_base * 1.20,
                       tratamento == 1 & periodo == 10 ~ vendas_base * 1.15,
                       TRUE ~ vendas_base),
    tratado_periodo = if_else(tratamento == 1 & periodo >= 7, 1, 0), # Dummy para tratados após o início do tratamento
    periodo_relativo = periodo - 7) %>% # Define períodos relativos para estudo de eventos
  arrange(loja, periodo) # ordena os dados por id da loja e periodo


# estatísticas descritivas sobre os dados
skimr::skim(dados)
str(dados)

pwartest(vendas ~ periodo, data = dados) # Teste de Wooldridge para autocorrelação

# Diff-in-Diff agregado
modelo_diff <- feols(vendas ~ tratado_periodo | loja + periodo, 
                     cluster = ~loja, data = dados)
summary(modelo_diff)

# Estudo de eventos (Event Study)
dados_event <- dados %>%
  mutate(periodo_relativo = factor(periodo_relativo, levels = sort(unique(periodo_relativo))))

modelo_event <- feols(vendas ~ i(periodo_relativo, tratamento, ref = "-1") | 
                        loja + periodo, cluster = ~loja, data = dados)
summary(modelo_event)

# Preparar os coeficientes para plotar o Event Study
event_study_results <- broom::tidy(modelo_event) %>%
  filter(term != "(Intercept)") %>%
  mutate(periodo = as.numeric(str_extract(term, "-?\\d+"))) %>%
  select(periodo, estimate, std.error) %>%
  mutate(
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

# Plot do Event Study
ggplot(event_study_results, aes(x = periodo, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Crescimento nas vendas, por período, nas lojas impactadas pela campanha OOH",
    x = "Período desde o início da campanha",
    y = "Efeito estimado nas vendas (milhões de R$)"
  ) +
  theme_minimal()

# Gráfico dos coeficientes do estudo de eventos
iplot(modelo_event, main = "Crescimento nas vendas, por período, nas lojas impactadas pela campanha OOH",
      xlab = 'Período desde o início da campanha',
      ylab = 'Efeito estimado nas vendas (milhões de R$)',
      xlab.cex = 1.2,
      ylab.cex = 1.2,
      main.cex = 1.4)

# Gráfico da média de vendas por estado e período
medias <- dados %>%
  group_by(periodo, estado) %>%
  summarise(vendas_media = mean(vendas), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(estado = factor(estado, levels = c("PE", "PB")))

ggplot(medias, aes(x = periodo, y = vendas_media, color = estado, group = estado)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 7, linetype = "dashed", size = 1) + # Marca o início do tratamento
  labs(
    title = "Média das vendas, em milhões de R$, por grupo (Tratamento [PB] vs Controle [PE])",
    x = 'Período',
    y = "Vendas médias (em milhões de R$)",
    color = "Estado"
  ) +
  scale_x_continuous(breaks = seq(min(medias$periodo), max(medias$periodo), by = 1)) + # Eixo x com números inteiros
  scale_color_manual(  # Adicione esta camada para inverter as cores
    values = c("PE" = "#fc81c7",  # Fuchsia para PE
               "PB" = "#1fd1ca")  # Teal para PB
  ) +
  theme_minimal(base_size = 16) + # Aumenta o tamanho do texto geral
  theme(
    legend.position = "bottom",          # Legenda abaixo do gráfico
    legend.title = element_text(size = 16), # Tamanho grande para o título da legenda
    legend.text = element_text(size = 14),  # Tamanho grande para os itens da legenda
    axis.title = element_text(size = 18),   # Aumenta o tamanho dos títulos dos eixos
    axis.text = element_text(size = 14),    # Aumenta o tamanho dos rótulos dos eixos
    plot.title = element_text(size = 20, hjust = 0.5) # Centraliza e aumenta o título
  )


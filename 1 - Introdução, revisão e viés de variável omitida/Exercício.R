
##########################################################
## Curso: INTRODUÇÃO À INFERÊNCIA CAUSAL APLICADA À INDÚSTRIA
## Instrutor: Robson Tigre
## Atividade: 1.3 EXERCÍCIO ENDOGENEIDADE E VARIÁVEL OMITIDA 
## Criação: 26.12.2024
## Última modificação: 26.12.2024
##########################################################

# Limpa ambiente
rm(list = ls())

######################################
# Exemplo de viés de variável omitida
######################################

# Especifica o caminho para o diretório de trabalho
setwd("/Users/rtigre/Desktop/Aulas IC/Aula 1")

# Lê dados
nls80 <- read.csv("nls80.csv", header = TRUE)

# Mostra estrutura dos dados
str(nls80)

# Estima primeira regressão
reg1 <- lm(wage ~ educ, data = nls80)
summary(reg1)

# Estima segunda regressão
reg2 <- lm(wage ~ educ + iq, data = nls80)
summary(reg2)

# Estima terceira regressão
reg3 <- lm(wage ~ educ + tenure, data = nls80)
summary(reg3)

# Matriz de correlação
cor(nls80[, c("educ", "iq", "tenure")])

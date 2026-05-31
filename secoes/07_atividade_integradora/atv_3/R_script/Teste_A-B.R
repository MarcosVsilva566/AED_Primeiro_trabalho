# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# ==========================================
# 1. CONFIGURAÇÃO DOS PARÂMETROS DO ESTUDO
# ==========================================
set.seed(42)

# Tamanho da amostra por grupo (visitantes)
n_A <- 2500 
n_B <- 2500

# Taxas de conversão REAIS (Desconhecidas no mundo real, mas fixadas para a simulação)
# Vamos assumir que o botão Verde (B) é de fato melhor
p_A_verdadeiro <- 0.10
p_B_verdadeiro <- 0.13 

# ==========================================
# 2. SIMULAÇÃO DO EXPERIMENTO (UM ÚNICO TESTE)
# ==========================================
# Simulando o número de conversões usando a distribuição Binomial
conv_A <- rbinom(n = 1, size = n_A, prob = p_A_verdadeiro)
conv_B <- rbinom(n = 1, size = n_B, prob = p_B_verdadeiro)

# Taxas observadas na amostra
p_A_obs <- conv_A / n_A
p_B_obs <- conv_B / n_B

cat("--- Resultados do Experimento Observado ---\n")
cat("Taxa de Conversão Grupo A (Controle):", round(p_A_obs * 100, 2), "%\n")
cat("Taxa de Conversão Grupo B (Tratamento):", round(p_B_obs * 100, 2), "%\n\n")

# ==========================================
# 3. INTERPRETAÇÃO E TESTE ESTATÍSTICO TEÓRICO
# ==========================================
# Aplicando o teste de proporções (Teste Z)
teste_ab <- prop.test(x = c(conv_A, conv_B), 
                      n = c(n_A, n_B), 
                      alternative = "two.sided", 
                      correct = FALSE) # Sem correção de Yates para alinhar com o Teorema Central do Limite puro

print(teste_ab)

# Interpretação Probabilística
p_valor <- teste_ab$p.value
if(p_valor < 0.05) {
  cat("\nConclusão: Rejeitamos H0 (p-valor =", p_valor, "). Há evidência estatística de que o botão Verde converte mais.\n")
} else {
  cat("\nConclusão: Falhamos em rejeitar H0 (p-valor =", p_valor, "). Não há evidência suficiente de diferença.\n")
}

# ==========================================
# 4. DISCREPÂNCIA ENTRE TEORIA E SIMULAÇÃO
# ==========================================
# Para validar o modelo teórico, vamos simular 10.000 testes A/B assumindo que H0 É VERDADEIRA 

n_simulacoes <- 10000
p_h0 <- 0.10

# Simulação de Monte Carlo vetorizada para velocidade
sim_A <- rbinom(n_simulacoes, n_A, p_h0) / n_A
sim_B <- rbinom(n_simulacoes, n_B, p_h0) / n_B
diferencas_simuladas <- sim_B - sim_A

# Parâmetros teóricos da Normal sob H0
erro_padrao_teorico <- sqrt(p_h0 * (1 - p_h0) * (1/n_A + 1/n_B))


df_simulacao <- data.frame(Diferenca = diferencas_simuladas)

# ==========================================
# 5. GRÁFICOS
# ==========================================

# Gráfico 1: Taxas de Conversão Observadas
df_barras <- data.frame(
  Grupo = c("A (Azul)", "B (Verde)"),
  Taxa = c(p_A_obs, p_B_obs)
)

g1 <- ggplot(df_barras, aes(x = Grupo, y = Taxa, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  geom_text(aes(label = scales::percent(Taxa, accuracy = 0.1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("steelblue", "seagreen")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15)) +
  labs(title = "Taxa de Conversão por Grupo",
       y = "Taxa de Conversão", x = "Grupo (Cor do Botão)") +
  theme_minimal() +
  theme(legend.position = "none")

print(g1)

# Gráfico 2: Teoria vs Simulação (Distribuição Nula)
g2 <- ggplot(df_simulacao, aes(x = Diferenca)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "black", alpha = 0.7) +

  stat_function(fun = dnorm, args = list(mean = 0, sd = erro_padrao_teorico), 
                color = "red", size = 1.2, linetype = "dashed") +

  geom_vline(xintercept = (p_B_obs - p_A_obs), color = "blue", size = 1.2) +
  labs(title = "Discrepância: Teoria vs. Simulação (Distribuição sob H0)",
       subtitle = "Histograma = Simulação de Monte Carlo | Linha Vermelha Tracejada = Distribuição Normal Teórica\nLinha Azul = Diferença observada no Teste A/B principal",
       x = "Diferença de Proporções (p_B - p_A)",
       y = "Densidade") +
  theme_minimal()

print(g2)

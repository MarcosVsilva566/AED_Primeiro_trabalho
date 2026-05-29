
# --- Carregamento de Pacotes ---


library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(xtable)

set.seed(1234)

# ==================================================
# Filtro Anti-Spam
# ==================================================
cat("\n========== SEÇÃO 1: FILTRO ANTI-SPAM ==========\n")


# Definição dos eventos:
# S: email é spam (S) ou não (N)
# F: filtro classifica como spam (positivo) ou não (negativo)

# Parâmetros iniciais:
  prev_spam <- 0.20     # Prevalência de spam
sens_filtro <- 0.95   # Sensibilidade: P(F+|S)
espec_filtro <- 0.90  # Especificidade: P(F-|N)

# Simulação de 10000 emails
n <- 10000
spam_real <- rbinom(n, 1, prev_spam)

# Aplicação do filtro (simulação condicional)
# Se for spam, probabilidade de classificar como spam = sens_filtro
# Se não for spam, probabilidade de classificar como spam = 1 - espec_filtro
filtro_pos <- rep(NA, n)
for (i in 1:n) {
  if (spam_real[i] == 1) {
    filtro_pos[i] <- rbinom(1, 1, sens_filtro)
  } else {
    filtro_pos[i] <- rbinom(1, 1, 1 - espec_filtro)
  }
}

# Matriz de confusão
confusion <- table(Real = ifelse(spam_real, "Spam", "Não Spam"),
                   Filtro = ifelse(filtro_pos, "Spam", "Não Spam"))
cat("Matriz de Confusão:\n")
print(confusion)

print(xtable(confusion, 
             caption = "Matriz de Confusão do Filtro de Spam", 
             label = "tab:matriz_confusao"), 
      type = "latex", 
      include.rownames = TRUE)


# Cálculos probabilísticos
# Probabilidade total de filtro positivo:
P_F_pos <- prev_spam * sens_filtro + (1 - prev_spam) * (1 - espec_filtro)
cat(sprintf("P(Filtro positivo) = %.4f\n", P_F_pos))

# Teorema de Bayes: Valor Preditivo Positivo (VPP) e Negativo (VPN)
VPP <- (prev_spam * sens_filtro) / P_F_pos
VPN <- ((1 - prev_spam) * espec_filtro) / (1 - P_F_pos)
cat(sprintf("VPP (P(Spam|F+)) = %.4f\n", VPP))
cat(sprintf("VPN (P(Não Spam|F-)) = %.4f\n", VPN))

# Gráfico: Barras da classificação real vs predita (apenas proporções)
df1 <- data.frame(
  Real = c("Spam", "Não Spam"),
  Predito_Spam = c(confusion[1,2]/sum(confusion[1,]), confusion[2,2]/sum(confusion[2,])),
  Predito_NaoSpam = c(confusion[1,1]/sum(confusion[1,]), confusion[2,1]/sum(confusion[2,]))
)
df1_long <- pivot_longer(df1, cols = starts_with("Predito"),
                         names_to = "Predito", values_to = "Proporcao")

g1 <- ggplot(df1_long, aes(x = Real, y = Proporcao, fill = Predito)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Filtro Anti-Spam: Proporções de Classificação",
       y = "Proporção", fill = "Classificação do Filtro") +
  theme_minimal()
print(g1)

cat("\n==================================================\n")

# ==================================================
# Sistema de Alarme
# ==================================================
cat("========== SEÇÃO 2: SISTEMA DE ALARMES ==========\n")

# Objetivo: Modelar a ocorrência de intrusão e falha elétrica,
# que afetam o disparo do alarme. Usar probabilidade total e Bayes.

# Definição dos eventos:
# I: intrusão (I) ou não (NI)
# F: falha elétrica (F) ou não (NF)
# A: alarme dispara (A) ou não (NA)

# Parâmetros:
P_I <- 0.01    # Probabilidade de intrusão
P_F <- 0.05    # Probabilidade de falha elétrica

# Probabilidades condicionais do alarme:
# P(A|I, F) = 0.99 (quase certo disparar se ambos)
# P(A|I, NF) = 0.95
# P(A|NI, F) = 0.10 (falso positivo por falha)
# P(A|NI, NF) = 0.001 (falso positivo raro)

# Simulação de 10000 situações
n2 <- 10000
intrusao <- rbinom(n2, 1, P_I)
falha <- rbinom(n2, 1, P_F)

alarme <- rep(NA, n2)
for (i in 1:n2) {
  if (intrusao[i]==1 & falha[i]==1) {
    alarme[i] <- rbinom(1,1,0.99)
  } else if (intrusao[i]==1 & falha[i]==0) {
    alarme[i] <- rbinom(1,1,0.95)
  } else if (intrusao[i]==0 & falha[i]==1) {
    alarme[i] <- rbinom(1,1,0.10)
  } else {
    alarme[i] <- rbinom(1,1,0.001)
  }
}

# Probabilidade total do alarme:
P_A <- P_I*P_F*0.99 + P_I*(1-P_F)*0.95 + (1-P_I)*P_F*0.10 + (1-P_I)*(1-P_F)*0.001
cat(sprintf("P(Alarme) = %.4f\n", P_A))

# Teorema de Bayes: P(I|A) - probabilidade de intrusão dado alarme
# Precisamos de P(A|I) = P(A|I,F)*P(F) + P(A|I,NF)*P(NF) (teorema da probabilidade total)
P_A_dado_I <- 0.99*P_F + 0.95*(1-P_F)
cat(sprintf("P(Alarme|Intrusao) = %.4f\n", P_A_dado_I))

P_I_dado_A <- (P_I * P_A_dado_I) / P_A
cat(sprintf("P(Intrusao|Alarme) = %.4f\n", P_I_dado_A))

# Interpretação:
cat(sprintf("Interpretação: Quando o alarme dispara, a probabilidade de intrusão é %.1f%%.\n", P_I_dado_A*100))

# Gráfico: Probabilidades conjuntas (diagrama de árvore simplificado) - heatmap
# Vamos mostrar P(Intrusao, Falha, Alarme) via simulação
df2 <- data.frame(
  Intrusao = ifelse(intrusao, "Sim", "Não"),
  Falha = ifelse(falha, "Sim", "Não"),
  Alarme = ifelse(alarme, "Sim", "Não")
)
df2_sum <- df2 %>%
  group_by(Intrusao, Falha, Alarme) %>%
  summarise(Contagem = n(), .groups = "drop") %>%
  mutate(Prob = Contagem / n2)

g2 <- ggplot(df2_sum, aes(x = Intrusao, y = Falha, fill = Prob)) +
  geom_tile() +
  facet_wrap(~Alarme) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Probabilidades Conjuntas (Intrusão, Falha, Alarme)",
       fill = "Probabilidade") +
  theme_minimal()
print(g2)

cat("\n==================================================\n")

# ==================================================
# SEÇÃO 3: Classificação Probabilística (Naive Bayes)
# ==================================================
cat("========== SEÇÃO 3: CLASSIFICAÇÃO PROBABILÍSTICA ==========\n")

# Objetivo: Implementar um classificador Bayes ingênuo simples
# com 3 classes e 2 atributos contínuos simulados.

# Parâmetros: Médias e desvios para cada classe (atributos X1 e X2)
classes <- c("C1", "C2", "C3")
prior <- c(0.3, 0.5, 0.2)  # Probabilidades a priori

# Parâmetros das distribuições condicionais (assumindo normal)
mu <- matrix(c(0, 0,  # C1: X1, X2
               3, 2,  # C2: X1, X2
               5, -1), nrow=3, byrow=TRUE)  # C3: X1, X2
sigma <- matrix(c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5), nrow=3, byrow=TRUE)  # desvios padrão

# Simular 500 pontos de treinamento
n_train <- 500
classes_verdade <- sample(classes, n_train, replace = TRUE, prob = prior)
X1 <- rep(NA, n_train)
X2 <- rep(NA, n_train)
for (i in 1:n_train) {
  idx <- which(classes == classes_verdade[i])
  X1[i] <- rnorm(1, mu[idx,1], sigma[idx,1])
  X2[i] <- rnorm(1, mu[idx,2], sigma[idx,2])
}
dados <- data.frame(Classe = classes_verdade, X1, X2)

# Função de densidade normal
dnorm2d <- function(x1, x2, mu1, mu2, s1, s2) {
  exp(-0.5*(((x1-mu1)/s1)^2 + ((x2-mu2)/s2)^2)) / (2*pi*s1*s2)
}

# Classificar um novo ponto (exemplo)
novo <- c(2, 1)  # (X1, X2)
post_probs <- rep(NA, 3)
for (k in 1:3) {
  likelihood <- dnorm2d(novo[1], novo[2], mu[k,1], mu[k,2], sigma[k,1], sigma[k,2])
  post_probs[k] <- prior[k] * likelihood
}
post_probs <- post_probs / sum(post_probs)
cat(sprintf("Ponto (%.1f, %.1f) - Probabilidades Posteriores:\n", novo[1], novo[2]))
for (k in 1:3) {
  cat(sprintf("  %s: %.3f\n", classes[k], post_probs[k]))
}
cat(sprintf("Classe predita: %s\n", classes[which.max(post_probs)]))

# Gráfico: Dispersão dos dados com fronteiras de decisão (usando grid)
# Criar grid de pontos
x1_range <- seq(min(X1)-1, max(X1)+1, length.out = 100)
x2_range <- seq(min(X2)-1, max(X2)+1, length.out = 100)
grid <- expand.grid(X1 = x1_range, X2 = x2_range)

# Calcular probabilidades para cada ponto do grid
grid_pred <- matrix(NA, nrow = nrow(grid), ncol = 3)
for (i in 1:nrow(grid)) {
  for (k in 1:3) {
    grid_pred[i,k] <- prior[k] * dnorm2d(grid$X1[i], grid$X2[i], mu[k,1], mu[k,2], sigma[k,1], sigma[k,2])
  }
  grid_pred[i,] <- grid_pred[i,] / sum(grid_pred[i,])
}
grid$Classe <- classes[apply(grid_pred, 1, which.max)]
grid$Prob <- apply(grid_pred, 1, max)

g3 <- ggplot(dados, aes(x = X1, y = X2, color = Classe)) +
  geom_point(size = 2) +
  geom_contour(data = grid, aes(z = Prob), bins = 1, color = "black", alpha = 0.5) +
  labs(title = "Classificação Bayes Ingênuo com Fronteiras") +
  theme_minimal()
print(g3)

cat("\n==================================================\n")

# ==================================================
# SEÇÃO 4: Autenticação Biométrica
# ==================================================
cat("========== SEÇÃO 4: AUTENTICAÇÃO BIOMÉTRICA ==========\n")

# Objetivo: Analisar taxas de FAR e FRR em um sistema biométrico,
# variando o limiar de decisão.

# Parâmetros: scores genuínos e impostores simulados
# Genuínos: distribuição normal com média 0.9 e desvio 0.1
# Impostores: distribuição normal com média 0.2 e desvio 0.1

n_genuino <- 1000
n_impostor <- 1000

scores_genuino <- rnorm(n_genuino, mean = 0.9, sd = 0.1)
scores_impostor <- rnorm(n_impostor, mean = 0.2, sd = 0.1)

# Limiares
limiares <- seq(0, 1, by = 0.01)

FAR <- rep(NA, length(limiares))
FRR <- rep(NA, length(limiares))
for (i in seq_along(limiares)) {
  t <- limiares[i]
  # FAR: impostores classificados como genuínos (score > t)
  FAR[i] <- mean(scores_impostor > t)
  # FRR: genuínos classificados como impostores (score <= t)
  FRR[i] <- mean(scores_genuino <= t)
}

# Cálculo da EER (Equal Error Rate) aproximado
idx_eer <- which.min(abs(FAR - FRR))
EER <- mean(c(FAR[idx_eer], FRR[idx_eer]))
cat(sprintf("EER (Equal Error Rate) aproximado: %.4f\n", EER))

# Gráfico: Curva ROC e trade-off FAR vs FRR
df4 <- data.frame(Limiar = limiares, FAR, FRR)
df4_long <- pivot_longer(df4, cols = c(FAR, FRR), names_to = "Taxa", values_to = "Valor")

g4 <- ggplot(df4_long, aes(x = Limiar, y = Valor, color = Taxa)) +
  geom_line(size = 1) +
  geom_vline(xintercept = limiares[idx_eer], linetype = "dashed", alpha = 0.5) +
  labs(title = "Curvas FAR e FRR vs Limiar",
       y = "Taxa") +
  theme_minimal()
print(g4)

# Interpretação:
cat(sprintf("Interpretação: Com limiar ótimo em %.2f, FAR = FRR = %.4f.\n", limiares[idx_eer], EER))

cat("\n==================================================\n")

# ==================================================
# SEÇÃO 5: Diagnóstico Médico
# ==================================================
cat("========== SEÇÃO 5: DIAGNÓSTICO MÉDICO ==========\n")

# Objetivo: Comparar VPP e VPN para diferentes prevalências de doença.

# Parâmetros do teste:
sens_teste <- 0.99  # Sensibilidade
espec_teste <- 0.95 # Especificidade

# Cenários de prevalência
prev <- c(0.01, 0.05, 0.10, 0.20, 0.50)

VPP_calc <- function(prev, sens, esp) {
  P_pos <- prev * sens + (1 - prev) * (1 - esp)
  VPP <- (prev * sens) / P_pos
  return(VPP)
}
VPN_calc <- function(prev, sens, esp) {
  P_neg <- prev * (1 - sens) + (1 - prev) * esp
  VPN <- ((1 - prev) * esp) / P_neg
  return(VPN)
}

vpps <- sapply(prev, VPP_calc, sens = sens_teste, esp = espec_teste)
vpns <- sapply(prev, VPN_calc, sens = sens_teste, esp = espec_teste)

# Simulação para um cenário específico (prevalência = 0.10)
n5 <- 10000
prev5 <- 0.10
doenca <- rbinom(n5, 1, prev5)
teste <- rep(NA, n5)
for (i in 1:n5) {
  if (doenca[i] == 1) {
    teste[i] <- rbinom(1, 1, sens_teste)
  } else {
    teste[i] <- rbinom(1, 1, 1 - espec_teste)
  }
}
# Matriz de confusão
conf5 <- table(Doença = ifelse(doenca, "Sim", "Não"),
               Teste = ifelse(teste, "Positivo", "Negativo"))
cat("Matriz de Confusão (Prevalência 10%):\n")
print(conf5)

# Gráfico: VPP e VPN vs Prevalência
df5 <- data.frame(Prevalencia = prev, VPP = vpps, VPN = vpns)
df5_long <- pivot_longer(df5, cols = c(VPP, VPN), names_to = "Medida", values_to = "Valor")

g5 <- ggplot(df5_long, aes(x = Prevalencia, y = Valor, color = Medida)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "VPP e VPN em função da Prevalência",
       y = "Probabilidade") +
  theme_minimal()
print(g5)

# Interpretação:
cat("Interpretação: O VPP aumenta com a prevalência; o VPN diminui.\n")
cat("Para doenças raras, mesmo testes com alta especificidade geram muitos falsos positivos.\n")

# ==================================================
# RESUMO DOS RESULTADOS
# ==================================================
cat("\n========== RESUMO DOS EXPERIMENTOS ==========\n")
cat("1. Filtro Anti-Spam: VPP = ", round(VPP,4), ", VPN = ", round(VPN,4), "\n")
cat("2. Sistema de Alarmes: P(Intrusão|Alarme) = ", round(P_I_dado_A,4), "\n")
cat("3. Classificação: Probabilidades posteriores para ponto (2,1): ", round(post_probs,3), "\n")
cat("4. Biometria: EER = ", round(EER,4), "\n")
cat("5. Diagnóstico Médico (prev=10%): VPP = ", round(vpps[3],4), ", VPN = ", round(vpns[3],4), "\n")
cat("\n==================================================\n")

# ==================================================
# COMO RELATAR NO TRABALHO (comentários)
# ==================================================
# Comentários para o aluno:
# 1. Descreva a metodologia utilizada em cada simulação, incluindo
#    as definições dos eventos, distribuições de probabilidade e
#    os parâmetros escolhidos.
# 2. Justifique a escolha dos parâmetros com base em situações
#    realistas (ex.: prevalência de spam entre 10-30%).
# 3. Apresente os resultados numéricos (probabilidades calculadas)
#    e os gráficos, explicando o que cada um representa.
# 4. Compare os cenários (ex.: diferentes prevalências no diagnóstico)
#    e discuta as limitações do modelo (ex.: independência condicional
#    no Naive Bayes).
# 5. Inclua uma seção de conclusão com os principais aprendizados
#    sobre o uso do Teorema de Bayes em situações práticas.
# 6. Referencie este script como material suplementar.
# Fim do script.

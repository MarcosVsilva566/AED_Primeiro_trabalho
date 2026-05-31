# T6: Atividade Integradora (Análise de Tráfego de Veículos)
set.seed(777)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

N <- 5000
lambda <- 4
mu <- 5

interchegadas <- rexp(N, rate = lambda)
tempos_servico <- rexp(N, rate = mu)

instantes_chegada <- cumsum(interchegadas)
instantes_inicio_servico <- numeric(N)
instantes_fim_servico <- numeric(N)

for(i in 1:N) {
  if(i == 1) {
    instantes_inicio_servico[i] <- instantes_chegada[i]
  } else {
    instantes_inicio_servico[i] <- max(instantes_chegada[i], instantes_fim_servico[i-1])
  }
  instantes_fim_servico[i] <- instantes_inicio_servico[i] + tempos_servico[i]
}

tempo_na_fila <- instantes_inicio_servico - instantes_chegada
df_trafego <- data.frame(EsperaFila = tempo_na_fila)

p_fila <- ggplot(df_trafego, aes(x = EsperaFila)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "firebrick3", color = "black", alpha = 0.6) +
  geom_vline(xintercept = mean(tempo_na_fila), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribuição do Tempo de Espera na Fila de Tráfego",
       x = "Tempo de Espera Retido na Fila (Minutos)",
       y = "Densidade") +
  theme_minimal()

ggsave("imagens/t6_analise_trafego.png", p_fila, width = 8, height = 5)

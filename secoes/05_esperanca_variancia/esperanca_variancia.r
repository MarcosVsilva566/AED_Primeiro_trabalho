# T4: Esperança, Variância e Lei dos Grandes Números
set.seed(123)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

lambda <- 0.2
K <- 10000
mu_teorica <- 1 / lambda

amostras <- rexp(K, rate = lambda)
medias_acumuladas <- cumsum(amostras) / (1:K)

df_lgn <- data.frame(Ensaio = 1:K, Media_Acumulada = medias_acumuladas)

p_lgn <- ggplot(df_lgn, aes(x = Ensaio, y = Media_Acumulada)) +
  geom_line(color = "royalblue", linewidth = 0.7) +
  geom_hline(yintercept = mu_teorica, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(title = "Estabilidade Estatística: Lei dos Grandes Números",
       x = "Número de Ensaios (Tamanho Amostral)",
       y = "Média Amostral Acumulada") +
  theme_minimal()

ggsave("imagens/t4_lei_grandes_numeros.png", p_lgn, width = 8, height = 5)

# T3: Distribuições Contínuas (Teorema Central do Limite)
set.seed(42)

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

simular_tcl <- function(dist = c("uniforme", "exponencial"), n_vals, M = 10000) {
  plots <- list()

  for(n in n_vals) {
    if(dist == "uniforme") {
      dados <- matrix(runif(M * n, min = 0, max = 1), nrow = M, ncol = n)
      mu_teorica <- 0.5
      sigma_teorica <- sqrt(1/12)
    } else {
      dados <- matrix(rexp(M * n, rate = 2), nrow = M, ncol = n)
      mu_teorica <- 0.5
      sigma_teorica <- 0.5
    }

    medias_amostrais <- rowMeans(dados)
    medias_padronizadas <- (medias_amostrais - mu_teorica) / (sigma_teorica / sqrt(n))
    df <- data.frame(z = medias_padronizadas)

    p <- ggplot(df, aes(x = z)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "seagreen3", color = "black", alpha = 0.6) +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "darkred", linewidth = 1) +
      labs(title = paste("n =", n), x = "Z", y = "Densidade") +
      theme_minimal() +
      xlim(-4, 4)

    plots[[paste("n", n, sep="_")]] <- p
  }
  return(plots)
}

plots_unif <- simular_tcl("uniforme", c(1, 2, 5, 30))
g_unif <- marrangeGrob(plots_unif, ncol = 2, nrow = 2, top = "Convergência da Distribuição Uniforme")
ggsave("imagens/t3_tcl_uniforme.png", g_unif, width = 8, height = 6)

plots_exp <- simular_tcl("exponencial", c(1, 2, 5, 30))
g_exp <- marrangeGrob(plots_exp, ncol = 2, nrow = 2, top = "Convergência da Distribuição Exponencial")
ggsave("imagens/t3_tcl_exponencial.png", g_exp, width = 8, height = 6)

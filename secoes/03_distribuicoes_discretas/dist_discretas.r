set.seed(42)

pasta <- "imagens"

# Cenario: falha de requisicoes em um servidor
p <- 0.05
n_req <- 20
lambda <- 2
n_rep <- 20000

# 3.1 Binomial: numero de falhas em n_req requisicoes
falhas_binom <- rbinom(n_rep, size = n_req, prob = p)
k_b <- 0:7
emp_binom <- sapply(k_b, function(k) mean(falhas_binom == k))
teo_binom <- dbinom(k_b, size = n_req, prob = p)

# 3.2 Poisson: numero de falhas por minuto
falhas_pois <- rpois(n_rep, lambda)
k_p <- 0:8
emp_pois <- sapply(k_p, function(k) mean(falhas_pois == k))
teo_pois <- dpois(k_p, lambda)

# 3.3 Convergencia da media amostral da Binomial para E[X] = n*p
media_acum <- cumsum(falhas_binom) / seq_len(n_rep)

print(round(c(media_binom = tail(media_acum, 1), teorico_binom = n_req * p), 4))
print(round(c(media_pois = mean(falhas_pois), teorico_pois = lambda), 4))

# Figura 1: Binomial empirica e teorica
png(file.path(pasta, "fig_binomial.png"), width = 1600, height = 1100, res = 200)
barplot(rbind(emp_binom, teo_binom), beside = TRUE, names.arg = k_b,
        col = c("steelblue", "tomato"), xlab = "Número de falhas",
        ylab = "Probabilidade", main = "Distribuição binomial empírica e teórica")
legend("topright", legend = c("Empírica", "Teórica"),
       fill = c("steelblue", "tomato"), bty = "n")
dev.off()

# Figura 2: Poisson empirica e teorica
png(file.path(pasta, "fig_poisson.png"), width = 1600, height = 1100, res = 200)
barplot(rbind(emp_pois, teo_pois), beside = TRUE, names.arg = k_p,
        col = c("steelblue", "tomato"), xlab = "Número de falhas",
        ylab = "Probabilidade", main = "Distribuição de Poisson empírica e teórica")
legend("topright", legend = c("Empírica", "Teórica"),
       fill = c("steelblue", "tomato"), bty = "n")
dev.off()

# Figura 3: convergencia da media amostral
png(file.path(pasta, "fig_media_discreta.png"), width = 1600, height = 1100, res = 200)
plot(media_acum, type = "l", col = "steelblue", xlab = "Número de repetições",
     ylab = "Média amostral acumulada",
     main = "Convergência da média amostral da binomial")
abline(h = n_req * p, col = "tomato", lty = 2, lwd = 2)
legend("topright", legend = c("Média acumulada", "Valor teórico"),
       col = c("steelblue", "tomato"), lty = c(1, 2), lwd = 2, bty = "n")
dev.off()

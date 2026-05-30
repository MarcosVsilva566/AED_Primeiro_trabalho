set.seed(42)

pasta <- "imagens"

# 2.1 Moeda
n_moeda <- 5000
result <- runif(n_moeda)
eh_cara <- result < 0.5
freq_rel <- cumsum(eh_cara) / seq_len(n_moeda)

print(freq_rel[5000])

png(file.path(pasta, "fig_moeda_convergencia.png"), width = 1600, height = 1100, res = 200)
plot(freq_rel, type = "l", col = "steelblue",
     xlab = "Número de lançamentos",
     ylab = "Frequência relativa de cara",
     main = "Convergência da frequência relativa de cara")
abline(h = 0.5, col = "red", lty = 2)
legend("bottomright", legend = c("Frequência empírica", "Probabilidade teórica (0,5)"),
       col = c("steelblue", "red"), lty = c(1, 2), bty = "n")
dev.off()

# 2.2 Urna
urna <- c(rep("vermelha", 5), rep("azul", 3), rep("verde", 2))
n_urna <- 8000
retiradas <- sample(urna, size = n_urna, replace = TRUE)

freq_emp_urna <- table(factor(retiradas, levels = c("vermelha", "azul", "verde"))) / n_urna
prob_teo_urna <- c(0.5, 0.3, 0.2)

print(freq_emp_urna)

comp_urna <- rbind(as.numeric(freq_emp_urna), prob_teo_urna)

png(file.path(pasta, "fig_urna_freq.png"), width = 1600, height = 1100, res = 200)
barplot(comp_urna, beside = TRUE, col = c("steelblue", "tomato"),
        names.arg = c("vermelha", "azul", "verde"),
        legend.text = c("Empírica", "Teórica"),
        xlab = "Cor da bola", ylab = "Frequência relativa",
        main = "Urna: frequência empírica e teórica")
dev.off()

# 2.3 Soma de dois dados
n_rep <- 20000
soma <- replicate(n_rep, sum(sample(1:6, size = 2, replace = TRUE)))

freq_emp_soma <- table(factor(soma, levels = 2:12)) / n_rep
freq_teo_soma <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1) / 36

comp_soma <- rbind(as.numeric(freq_emp_soma), freq_teo_soma)

png(file.path(pasta, "fig_soma_dois_dados.png"), width = 1600, height = 1100, res = 200)
barplot(comp_soma, beside = TRUE, col = c("steelblue", "tomato"),
        names.arg = 2:12,
        legend.text = c("Empírica", "Teórica"),
        xlab = "Soma dos dois dados", ylab = "Frequência relativa",
        main = "Soma de dois dados: empírica e teórica")
dev.off()

media_soma <- cumsum(soma) / seq_len(n_rep)

print(media_soma[n_rep])

png(file.path(pasta, "fig_soma_media.png"), width = 1600, height = 1100, res = 200)
plot(media_soma, type = "l", col = "steelblue",
     xlab = "Número de repetições",
     ylab = "Média amostral acumulada",
     main = "Convergência da média amostral da soma")
abline(h = 7, col = "red", lty = 2)
legend("topright", legend = c("Média empírica", "Valor esperado (7)"),
       col = c("steelblue", "red"), lty = c(1, 2), bty = "n")
dev.off()

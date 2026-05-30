set.seed(42)

pasta <- "imagens"

# Modelo gerador das mensagens
palavras <- c("gratis", "promocao", "clique", "dinheiro", "urgente",
              "conta", "reuniao", "projeto", "relatorio", "ola")

prob_spam     <- c(0.60, 0.50, 0.55, 0.45, 0.40, 0.35, 0.05, 0.04, 0.03, 0.30)
prob_nao_spam <- c(0.05, 0.08, 0.10, 0.05, 0.15, 0.20, 0.40, 0.45, 0.35, 0.50)

prior_spam     <- 0.40
prior_nao_spam <- 0.60

gerar_emails <- function(n) {
  classe <- sample(c("spam", "nao_spam"), size = n, replace = TRUE,
                   prob = c(prior_spam, prior_nao_spam))
  X <- matrix(0L, nrow = n, ncol = length(palavras))
  for (j in seq_along(palavras)) {
    p <- ifelse(classe == "spam", prob_spam[j], prob_nao_spam[j])
    X[, j] <- rbinom(n, size = 1, prob = p)
  }
  list(classe = classe, X = X)
}

n_treino <- 4000
n_teste  <- 2000
treino <- gerar_emails(n_treino)
teste  <- gerar_emails(n_teste)

# Treinamento
prior_spam_est     <- mean(treino$classe == "spam")
prior_nao_spam_est <- 1 - prior_spam_est

estimar_prob <- function(X, classe, alvo) {
  Xc <- X[classe == alvo, , drop = FALSE]
  colSums(Xc) / nrow(Xc)
}
prob_spam_est     <- estimar_prob(treino$X, treino$classe, "spam")
prob_nao_spam_est <- estimar_prob(treino$X, treino$classe, "nao_spam")

print(round(prior_spam_est, 4))

# Classificacao
verossimilhanca <- function(X, prob) {
  apply(X, 1, function(x) prod(prob^x * (1 - prob)^(1 - x)))
}
posterior_spam <- function(X) {
  peso_spam     <- prior_spam_est     * verossimilhanca(X, prob_spam_est)
  peso_nao_spam <- prior_nao_spam_est * verossimilhanca(X, prob_nao_spam_est)
  peso_spam / (peso_spam + peso_nao_spam)
}

post_teste <- posterior_spam(teste$X)
pred <- ifelse(post_teste > 0.5, "spam", "nao_spam")

n_nao_spam <- sum(teste$classe == "nao_spam")
n_spam     <- sum(teste$classe == "spam")
fp <- sum(teste$classe == "nao_spam" & pred == "spam")
fn <- sum(teste$classe == "spam"     & pred == "nao_spam")

print(round(mean(pred == teste$classe), 4))
print(round(fp / n_nao_spam, 4))
print(round(fn / n_spam, 4))
print(table(real = teste$classe, predito = factor(pred, levels = c("nao_spam", "spam"))))

# Figura 1
faixas <- seq(0, 1, by = 0.05)
h_nao_spam <- hist(post_teste[teste$classe == "nao_spam"], breaks = faixas, plot = FALSE)
h_spam     <- hist(post_teste[teste$classe == "spam"],     breaks = faixas, plot = FALSE)
ymax       <- max(h_nao_spam$counts, h_spam$counts)

linha_hist <- function(h, cor) {
  x <- rep(h$breaks, each = 2)
  y <- c(0, rep(h$counts, each = 2), 0)
  lines(x, y, col = cor, lwd = 2)
}

png(file.path(pasta, "fig_spam_posterior.png"), width = 1600, height = 1100, res = 200)
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, ymax),
     xlab = "Probabilidade a posteriori P(spam | mensagem)", ylab = "Frequência",
     main = "Separação das classes pela probabilidade a posteriori")
linha_hist(h_nao_spam, "steelblue")
linha_hist(h_spam, "tomato")
abline(v = 0.5, lty = 2)
legend("top", legend = c("Não spam", "Spam", "Limiar 0,5"),
       col = c("steelblue", "tomato", "black"), lwd = c(2, 2, 1),
       lty = c(1, 1, 2), bty = "n")
dev.off()

# Figura 2
limiares <- seq(0, 1, by = 0.01)
taxa_fp <- sapply(limiares, function(t) sum(teste$classe == "nao_spam" & post_teste > t) / n_nao_spam)
taxa_fn <- sapply(limiares, function(t) sum(teste$classe == "spam"     & post_teste <= t) / n_spam)

png(file.path(pasta, "fig_spam_limiar.png"), width = 1600, height = 1100, res = 200)
plot(limiares, taxa_fp, type = "l", col = "steelblue", lwd = 2, ylim = c(0, 1),
     xlab = "Limiar de decisão", ylab = "Taxa de erro",
     main = "Equilíbrio entre falsos positivos e falsos negativos")
lines(limiares, taxa_fn, col = "tomato", lwd = 2)
abline(v = 0.5, lty = 2)
legend("top", legend = c("Falso positivo", "Falso negativo", "Limiar 0,5"),
       col = c("steelblue", "tomato", "black"), lwd = c(2, 2, 1),
       lty = c(1, 1, 2), bty = "n")
dev.off()

# Figura 3
x <- teste$X[which(teste$classe == "spam")[1], ]
acum_spam     <- prior_spam_est
acum_nao_spam <- prior_nao_spam_est
post_seq <- prior_spam_est
for (j in seq_along(palavras)) {
  if (x[j] == 1) {
    acum_spam     <- acum_spam     * prob_spam_est[j]
    acum_nao_spam <- acum_nao_spam * prob_nao_spam_est[j]
  } else {
    acum_spam     <- acum_spam     * (1 - prob_spam_est[j])
    acum_nao_spam <- acum_nao_spam * (1 - prob_nao_spam_est[j])
  }
  post_seq <- c(post_seq, acum_spam / (acum_spam + acum_nao_spam))
}

png(file.path(pasta, "fig_spam_sequencial.png"), width = 1600, height = 1100, res = 200)
par(mar = c(9, 4.5, 4, 2))
plot(0:length(palavras), post_seq, type = "b", pch = 19, col = "steelblue", lwd = 2,
     ylim = c(0, 1), xaxt = "n", xlab = "",
     ylab = "P(spam | palavras observadas)",
     main = "Probabilidade de spam atualizada a cada palavra")
axis(1, at = 0:length(palavras), labels = c("priori", paste0(palavras, "=", x)),
     las = 2, cex.axis = 0.7)
mtext("Evidência acumulada (palavra a palavra)", side = 1, line = 7.5)
abline(h = 0.5, lty = 2)
legend("bottomright", legend = c("P(spam) acumulada", "Limiar 0,5"),
       col = c("steelblue", "black"), lwd = c(2, 1), lty = c(1, 2),
       pch = c(19, NA), bty = "n")
dev.off()

print(round(tail(post_seq, 1), 3))

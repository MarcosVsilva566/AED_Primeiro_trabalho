set.seed(42)

pasta <- "imagens"

# Rede com dois grupos de 40 nos (modelo de blocos estocasticos)
n <- 80
grupo <- c(rep(1, 40), rep(2, 40))
p_in <- 0.25
p_out <- 0.05

# Matriz de probabilidade de aresta segundo os grupos dos nos
prob_aresta <- ifelse(outer(grupo, grupo, "=="), p_in, p_out)

# Gera a rede completa (nao direcionada, sem lacos)
adj_full <- matrix(0, n, n)
ind <- lower.tri(adj_full)
adj_full[ind] <- rbinom(sum(ind), 1, prob_aresta[ind])
adj_full <- adj_full + t(adj_full)

# Oculta 20% das arestas existentes para simular as nao observadas
arestas <- which(lower.tri(adj_full) & adj_full == 1)
ocultas <- sample(arestas, round(0.2 * length(arestas)))
adj_obs <- adj_full
adj_obs[ocultas] <- 0
adj_obs[upper.tri(adj_obs)] <- 0
adj_obs <- adj_obs + t(adj_obs)

# Escores de predicao para os pares nao observados
viz_comuns <- adj_obs %*% adj_obs
grau <- rowSums(adj_obs)
pares <- which(upper.tri(adj_obs) & adj_obs == 0)
cn <- viz_comuns[pares]
gi <- grau[row(adj_obs)[pares]]
gj <- grau[col(adj_obs)[pares]]
jaccard <- ifelse(gi + gj - cn == 0, 0, cn / (gi + gj - cn))
rotulo <- adj_full[pares]

# Area sob a curva ROC pela estatistica de postos
auc <- function(escore, rotulo) {
  r <- rank(escore)
  n_pos <- sum(rotulo == 1)
  n_neg <- sum(rotulo == 0)
  (sum(r[rotulo == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}
auc_cn <- auc(cn, rotulo)
auc_jac <- auc(jaccard, rotulo)

print(round(c(auc_vizinhos = auc_cn, auc_jaccard = auc_jac), 4))

# Pontos da curva ROC para um escore
curva_roc <- function(escore, rotulo) {
  ord <- order(escore, decreasing = TRUE)
  rot <- rotulo[ord]
  list(fpr = c(0, cumsum(1 - rot) / sum(1 - rot)),
       tpr = c(0, cumsum(rot) / sum(rot)))
}
roc_cn <- curva_roc(cn, rotulo)
roc_jac <- curva_roc(jaccard, rotulo)

# Figura: curvas ROC dos dois escores
png(file.path(pasta, "fig_enlaces_roc.png"), width = 1600, height = 1100, res = 200)
plot(roc_cn$fpr, roc_cn$tpr, type = "l", col = "steelblue", lwd = 2,
     xlim = c(0, 1), ylim = c(0, 1), xlab = "Taxa de falsos positivos",
     ylab = "Taxa de verdadeiros positivos",
     main = "Curvas ROC para predição de enlaces")
lines(roc_jac$fpr, roc_jac$tpr, col = "tomato", lwd = 2)
abline(0, 1, lty = 2)
legend("bottomright",
       legend = c("Vizinhos comuns", "Coeficiente de Jaccard", "Aleatório"),
       col = c("steelblue", "tomato", "black"), lwd = c(2, 2, 1),
       lty = c(1, 1, 2), bty = "n")
dev.off()

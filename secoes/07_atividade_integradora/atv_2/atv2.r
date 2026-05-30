set.seed(42)

pasta <- "imagens"

lambda <- 0.1
N <- 100000

comp <- matrix(rexp(N * 3, rate = lambda), nrow = N, ncol = 3)

vida_unico <- comp[, 1]
vida_serie <- pmin(comp[, 1], comp[, 2], comp[, 3])
vida_paral <- pmax(comp[, 1], comp[, 2], comp[, 3])

tempo_medio <- rbind(
  Empirico = c(mean(vida_unico), mean(vida_serie), mean(vida_paral)),
  Teorico  = c(1 / lambda, 1 / (3 * lambda), (1 / lambda) * sum(1 / 1:3))
)
colnames(tempo_medio) <- c("unico", "serie3", "paralelo3")
print(round(tempo_medio, 4))

# Figura 1
t_grid <- seq(0, 40, by = 0.5)
R_emp  <- function(vida) sapply(t_grid, function(t) mean(vida > t))

png(file.path(pasta, "fig_conf_curvas.png"), width = 1600, height = 1100, res = 200)
plot(t_grid, R_emp(vida_unico), pch = 20, cex = 0.6, col = "steelblue", ylim = c(0, 1),
     xlab = "Tempo t", ylab = "Confiabilidade R(t) = P(T > t)",
     main = "Confiabilidade de arranjos de componentes")
points(t_grid, R_emp(vida_serie), pch = 20, cex = 0.6, col = "tomato")
points(t_grid, R_emp(vida_paral), pch = 20, cex = 0.6, col = "darkgreen")
curve(exp(-lambda * x),             add = TRUE, col = "steelblue", lwd = 2)
curve(exp(-3 * lambda * x),         add = TRUE, col = "tomato",    lwd = 2)
curve(1 - (1 - exp(-lambda * x))^3, add = TRUE, col = "darkgreen", lwd = 2)
legend("topright",
       legend = c("Componente único", "Série de 3", "Paralelo de 3"),
       col = c("steelblue", "tomato", "darkgreen"), lwd = 2, bty = "n")
dev.off()

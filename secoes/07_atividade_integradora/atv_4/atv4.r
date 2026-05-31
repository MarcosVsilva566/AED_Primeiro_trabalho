set.seed(42)

pasta <- "imagens"

# Estados (contexto, inclui o inicio ^) e saidas (proximo caractere, inclui o fim $)
estados <- c(letters, "^")
saidas  <- c(letters, "$")

# Lista sintetica de treino (apenas letras minusculas)
treino <- c("senha", "brasil", "flamengo", "palmeiras", "gremio", "password",
            "admin", "usuario", "abcd", "qwerty", "asdfgh", "zxcvbn", "monteiro",
            "lobato", "paodeacucar", "maracana", "copacabana", "cachorro", "gato",
            "gatinho", "amor", "saudade", "felicidade", "brasilia", "carioca",
            "mineiro", "baiano", "pernambuco", "computador", "internet", "programa",
            "linguagem", "codigo", "futebol", "escola", "trabalho", "familia",
            "teclado", "janela", "aluno")

# Estima a matriz de transicao de ordem 2 com suavizacao de Laplace
alpha <- 0.01
cont <- array(alpha, dim = c(length(estados), length(estados), length(saidas)),
              dimnames = list(estados, estados, saidas))
for (w in treino) {
  w_ext <- c("^", "^", strsplit(w, "")[[1]], "$")
  for (i in seq_len(length(w_ext) - 2)) {
    cont[w_ext[i], w_ext[i + 1], w_ext[i + 2]] <-
      cont[w_ext[i], w_ext[i + 1], w_ext[i + 2]] + 1
  }
}
probs <- sweep(cont, 1:2, apply(cont, 1:2, sum), FUN = "/")

# Gera uma senha caractere a caractere ate sortear o fim ($)
gerar_senha <- function(probs) {
  c1 <- "^"; c2 <- "^"; senha <- character(0)
  for (k in 1:16) {
    prox <- sample(saidas, 1, prob = probs[c1, c2, ])
    if (prox == "$") break
    senha <- c(senha, prox)
    c1 <- c2; c2 <- prox
  }
  paste(senha, collapse = "")
}

n_senhas <- 5000
senhas <- replicate(n_senhas, gerar_senha(probs))

# Log-probabilidade de cada senha sob o modelo
log_prob <- function(senha, probs) {
  w_ext <- c("^", "^", strsplit(senha, "")[[1]], "$")
  total <- 0
  for (i in seq_len(length(w_ext) - 2)) {
    total <- total + log(probs[w_ext[i], w_ext[i + 1], w_ext[i + 2]])
  }
  total
}
log_probs <- sapply(senhas, log_prob, probs = probs)

# Entropia estimada do modelo e limite de forca bruta (em bits)
entropia <- -mean(log_probs) / log(2)
compr_medio <- mean(nchar(senhas))
forca_bruta <- compr_medio * log2(26)

print(round(c(entropia_bits = entropia, forca_bruta_bits = forca_bruta,
              comprimento_medio = compr_medio), 2))

# Figura: distribuicao da log-probabilidade das senhas geradas
png(file.path(pasta, "fig_senhas_logprob.png"), width = 1600, height = 1100, res = 200)
hist(log_probs, breaks = 40, col = "steelblue", border = "white",
     xlab = "Log-probabilidade da senha", ylab = "Frequência",
     main = "Distribuição da log-probabilidade das senhas geradas")
abline(v = mean(log_probs), col = "tomato", lty = 2, lwd = 2)
legend("topleft", legend = "Log-probabilidade média",
       col = "tomato", lty = 2, lwd = 2, bty = "n")
dev.off()

# --- Questão 1 ---

# 1. Dados da Tabela 1
tempos <- c(0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84, 2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27)

# 2. Cálculo do MLE
n <- length(tempos)
soma_x <- sum(tempos)
lambda_hat <- n / soma_x # O inverso da média
print(paste("Estimador MLE (lambda):", round(lambda_hat, 4)))

# 3. Gráfico da Log-Verossimilhança
# Definindo a função l(lambda)
log_lik <- function(lambda, dados) {
  n <- length(dados)
  soma <- sum(dados)
  return(n * log(lambda) - lambda * soma)
}

# Criando sequência de valores para lambda para o eixo X
lambdas_vals <- seq(0.01, 0.5, length.out = 100)
log_vals <- sapply(lambdas_vals, log_lik, dados = tempos)

# Plotando
plot(lambdas_vals, log_vals, type = "l", col = "blue", lwd = 2,
     main = "Função Log-Verossimilhança l(lambda)",
     xlab = "Lambda", ylab = "Log-Likelihood")
abline(v = lambda_hat, col = "red", lty = 2) # Linha vertical no estimador
legend("bottomright", legend = c("l(lambda)", "MLE"), col = c("blue", "red"), lty = c(1, 2))

# 4. Tempo médio estimado e Probabilidade P(X > 5)
media_estimada <- 1 / lambda_hat
prob_maior_5 <- exp(-5 * lambda_hat) # usando a fórmula teórica da exponencial
# Alternativamente usando pexp: 1 - pexp(5, rate = lambda_hat)

print(paste("Tempo médio estimado:", round(media_estimada, 4), "anos"))
print(paste("Probabilidade P(X > 5):", round(prob_maior_5, 4)))


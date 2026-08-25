#Questão 3 - Temperaturas cpu multicore

# 1. Função Box-Muller

# Função para gerar n valores normais via Box-Muller

gerar_temperatura_cpu <- function(n) {
  
  # (a) Gerar variáveis uniformes
  n_pares <- ceiling(n / 2)
  u1 <- runif(n_pares, min = 0, max = 1)
  u2 <- runif(n_pares, min = 0, max = 1)
  
  # (b) Fórmulas de Box-Muller para Normal Padrão (Z)
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  
  # Concatenar para formar vetor Z
  z <- c(z1, z2)
  z <- z[1:n]
  
  # (c) Converter para a distribuição de temperatura T = mu + sigma * Z
  mu <- 62
  sigma <- 3.5
  t_cpu <- mu + (sigma * z)
  
  return(t_cpu)
}

# 2. Gerando Medições

set.seed(123) 
n_amostras <- 1000
mu <- 62
sigma <- 3.5

# Conjunto 1: RNG Manual (Box-Muller)
dados_manual <- gerar_temperatura_cpu(n_amostras)

# Conjunto 2: RNG do R (rnorm)
dados_r <- rnorm(n_amostras, mean = mu, sd = sigma)


# 3. Análise Estatística


# Função auxiliar para calcular estatísticas
analisar_dados <- function(dados, nome) {
  media <- mean(dados)
  desvio <- sd(dados)
  minimo <- min(dados)
  maximo <- max(dados)
  
  # Probabilidades Empíricas
  prob_maior_68 <- mean(dados > 68)
  prob_entre_60_65 <- mean(dados > 60 & dados < 65)
  prob_maior_75 <- mean(dados > 75)
  
  return(c(Media=media, SD=desvio, Min=minimo, Max=maximo, 
           P_T_maior_68=prob_maior_68, P_60_menor_T_menor_65=prob_entre_60_65, 
           P_T_maior_75=prob_maior_75))
}

# Cálculos Teóricos (Usando a função pnorm do R)
# P(T > 68) = 1 - P(T <= 68)
teo_maior_68 <- 1 - pnorm(68, mean=mu, sd=sigma)
# P(60 < T < 65) = P(T < 65) - P(T < 60)
teo_entre_60_65 <- pnorm(65, mean=mu, sd=sigma) - pnorm(60, mean=mu, sd=sigma)
# P(T > 75)
teo_maior_75 <- 1 - pnorm(75, mean=mu, sd=sigma)

# Exibindo Resultados
stats_manual <- analisar_dados(dados_manual, "Manual")
stats_r <- analisar_dados(dados_r, "R Built-in")

# tabela comparativa
tabela_resultados <- data.frame(
  Metrica = c("Média", "Desvio Padrão", "Mínimo", "Máximo", 
              "P(T > 68)", "P(60 < T < 65)", "P(T > 75)"),
  Teorico = c(mu, sigma, "-inf", "+inf", 
              round(teo_maior_68, 4), round(teo_entre_60_65, 4), round(teo_maior_75, 6)),
  Manual = round(stats_manual, 4),
  R_Builtin = round(stats_r, 4)
)

print("Tabela Comparativa de Estatísticas")
print(tabela_resultados)

# Verificação de valores extremos (> 75)
contagem_extremos_manual <- sum(dados_manual > 75)
contagem_extremos_r <- sum(dados_r > 75)
cat("\nValores acima de 75°C (Manual):", contagem_extremos_manual)
cat("\nValores acima de 75°C (R):", contagem_extremos_r, "\n")

# 4. Visualização

par(mfrow = c(1, 2)) # Configura área de plotagem (1 linha, 2 colunas)

# Histograma Manual
hist(dados_manual, probability = TRUE, col = "lightblue", 
     main = "Histograma: RNG Manual (Box-Muller)",
     xlab = "Temperatura (°C)", ylim = c(0, 0.12), breaks=20)
curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, col="red", lwd=2)

# Histograma padrão do R
hist(dados_r, probability = TRUE, col = "lightgreen", 
     main = "Histograma: RNG Nativo do R",
     xlab = "Temperatura (°C)", ylim = c(0, 0.12), breaks=20)
curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, col="blue", lwd=2, lty=2)

par(mfrow = c(1, 1)) # Restaura plotagem padrão


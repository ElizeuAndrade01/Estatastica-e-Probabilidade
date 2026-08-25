#Questão 2

n <- 10000000
p <- 10^-7

#Como há uma grande quantidade de tentativas e chances pequenas de sucesso, prosseguiremos utilizando Poisson.

lambda <- n*p

#1.Aproximação:

x_valores <- 0:10
prob_poisson <- dpois(x_valores, lambda)

print("Aproximação Poisson (PMF)")
print(data.frame(vencedores = x_valores, probabilidade = round(prob_poisson, 6)))

#Com o n tão grande e p tão pequeno, a Bin(n,p) = Pois(lambda), portanto
#pela discrepância ser tão grande a aproximação vai ser ótima.

#2. valor esperado, Variância (exata e aproximada)

# Exata (Binomial)

E_exata <- n * p
Var_exata <- n * p * (1 - p)

# Aproximada (Poisson)

E_aprox <- lambda
Var_aprox <- lambda

print("--- ITEM 2: Comparação de Momentos ---")
cat(sprintf("Média Exata (Bin): %.10f | Média Aprox (Pois): %.10f\n", E_exata, E_aprox))
cat(sprintf("Var Exata (Bin)  : %.10f | Var Aprox (Pois)  : %.10f\n", Var_exata, Var_aprox))

#Os resultados são quase idênticos porque (1-p) é quase igual a 1.

#3. Probabilidade recompensa (Dado que ganhei)

# W ~ Pois(1) é o número de OUTROS vencedores.
# Total de vencedores = 1 (eu) + W.
# Se houver (1+W) vencedores, minha chance de levar o prêmio é 1 / (1+W).
# calcularei então o valor esperado: E[ 1 / (1+W) ]

# Vamos somar as probabilidades para os primeiros termos (a probabilidade decai rápido)
ganhadores_valores <- 0:20 
prob_ganhar <- dpois(ganhadores_valores, lambda = 1) # P(W = k)
recompensas <- 1 / (ganhadores_valores + 1)

prob_receber_premio <- sum(recompensas * prob_ganhar)

print("Probabilidade de Receber o Prêmio")
cat("Probabilidade calculada:", prob_receber_premio, "\n")

#4. Simulação e Comparação visual.

set.seed(123)
n_simulacoes <- 10000

simulacao <- rbinom(n_simulacoes, size = n, prob = p)

#Binomial
tabela <- table(factor(simulacao, levels = 0:10)) / n_simulacoes
plot(0:10, tabela, type = "h", lwd = 5, col = "grey",
     main = "Simulação (Binomial) vs Teoria (Poisson)",
     xlab = "Número de Vencedores", ylab = "Probabilidade")

#poisson
points(0:10, dpois(0:10, lambda), col = "red", pch = 19)
lines(0:10, dpois(0:10, lambda), col = "red", lty = 2)



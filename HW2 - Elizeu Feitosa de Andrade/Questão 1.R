#Questão 1

n <- 50
p <- 0.7

#1. Determinando a função de distribuição de X:
# X segue uma distribuição binomial utilizando 2 parâmetros (numero de clientes: 50 e Probabilidade de sucesso, ou seja, pedir sobremesa: 0.7)
#Portanto
 
x_valores <- 0:n
probabilidades <- dbinom(x_valores, size = n, prob = p)
 
#2. Construindo gráficos PMF e CDF de X.
 
#Configurando área de plotagem para 2 gráficos
par(mfrow = c(1,2))

#Gráfico 1:
#PMF

plot(x_valores, dbinom(x_valores, n, p), type = "h", main = "PMF - Função Massa de Probabilidade", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#264653", lwd = 2)
points(x_valores, dbinom(x_valores, n, p), pch = 16, col = "#2A9D8F", cex = 0.6)

#Gráfico 2 - CDF:

plot(x_valores, pbinom(x_valores, n, p), type = "s", main = "CDF - Função Distribuição Acumulada", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#eb4d4b", cex = 0.6)

#3. Esperança, variância e desvio padrão:

esperanca <- n*p
variancia <- n*p*(1-p)
desvio_padrao <- sqrt(variancia)

cat("Esperança: ", esperanca, "\n")
cat("Variancia: ", variancia, "\n")
cat("Desvio Padrão: ", desvio_padrao, "\n")

#4. Calculando as probabilidades

#a. P(X>=20)
prob_maior_20 <- 1 - pbinom(19, n, p)
cat("a) P(X>=20) = ", prob_maior_20)

#b. P(30<X<43)
prob_maior_30_menor_43 <- pbinom(43, n, p) - pbinom(30, n, p)
cat("b) P(30<X<43): ", prob_maior_30_menor_43)

#c. P(X = 31)
prob_31 <- dbinom(31, n, p)
cat("c) P(X = 31): ", prob_31)

#5.R: Bom, intuitivamente a vontade de estocar seria estocar a média para tentar economizar ou evitar desperdício
#Mas isso apenas supriria parte da demanda, o ideal seria pegar o valor onde X = k seja próximo dos 85 a 95% de
#probabilidade, Segundo o CDF podemos ver que se encontraria mais pelas 40 sobremesas, dessa forma, se faltasse
#seriam poucas unidades, e se sobrasse, também seriam poucas unidades, eliminando o potencial desperdício caso fosse
#comprado a quantidade de sobremesa de acordo com a quantidade de clientes.

#6.R: Para ver as possíveis mudanças que podem ocorrer em um gráfico deste caso se p ou n, ou ambos mudarem
#podemos fazer algumas plotagens.

#n = 50, p = 0.8
p <- 0.8

#Gráfico 1:
#PMF

plot(x_valores, dbinom(x_valores, n, p), type = "h", main = "PMF - Função Massa de Probabilidade", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#264653", lwd = 2)
points(x_valores, dbinom(x_valores, n, p), pch = 16, col = "#2A9D8F", cex = 0.6)

#Gráfico 2 - CDF:

plot(x_valores, pbinom(x_valores, n, p), type = "s", main = "CDF - Função Distribuição Acumulada", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#eb4d4b", cex = 0.6)

#Mudando apenas a probabilidade de sucesso para 0.8, podemos ver que em ambos os gráficos a concentração de dados se
#moveram para a direita deixando o gráfico ainda mais assimétrico e com a calda ainda maior a esquerda.
#Mas se voltarmos p = 0.7 e aumentarmos a população do suporte, veremos o que acontece.

#n = 500, p = 0.7
p <- 0.7
n <- 500
x_valores <- 0:n

#Gráfico 1:
#PMF

plot(x_valores, dbinom(x_valores, n, p), type = "h", main = "PMF - Função Massa de Probabilidade", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#264653", lwd = 2)
points(x_valores, dbinom(x_valores, n, p), pch = 16, col = "#2A9D8F", cex = 0.6)

#Gráfico 2 - CDF:

plot(x_valores, pbinom(x_valores, n, p), type = "s", main = "CDF - Função Distribuição Acumulada", xlab = "Sucessos (k)", ylab = "P(X = k)", col = "#eb4d4b", cex = 0.6)

#Pudemos ver que com o aumento da população a o abrangimento de população do sino da normal também aumentou mas também
#se estreitou, se continuarmos aumentando a população, o sino irá ficando cada vez mais afunilado, deixei um exemplo
#extremo pada podermos observar mais facilmente.

#Configurando área de plotagem para a padrão
par(mfrow = c(1,1))


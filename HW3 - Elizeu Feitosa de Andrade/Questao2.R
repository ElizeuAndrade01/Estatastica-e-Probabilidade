# --- Questão 2 ---
library(palmerpenguins)

# 1. Preparação dos dados
# Removendo dados faltantes
penguins_data <- na.omit(penguins)

# Definindo variaveis
x <- penguins_data$body_mass_g   # Independente
y <- penguins_data$bill_length_mm # Dependente

# Grafico de Dispersão
plot(x, y, pch = 19, col = "darkgray",
     main = "Regressão: Massa Corporal vs Comprimento do Bico",
     xlab = "Massa Corporal (g)", ylab = "Comprimento do Bico (mm)")

# 2. Ajuste do Modelo Linear
modelo <- lm(bill_length_mm ~ body_mass_g, data = penguins_data)
summary(modelo) # Mostrando coeficientes

# Adicionando reta ao grafico
abline(modelo, col = "blue", lwd = 2)

# 3. Residuos e Metricas
residuos <- residuals(modelo)
plot(residuos, main = "Resíduos da Regressão", ylab = "Resíduo")
abline(h = 0, col = "red")

# Calculo manual do RMSE
rmse <- sqrt(mean(residuos^2))
r_squared <- summary(modelo)$r.squared

print(paste("RMSE Original:", round(rmse, 4)))
print(paste("R2 Original:", round(r_squared, 4)))

# 4. Introduzindo um Outlier Artificial
# Criando uma cópia e adicionando um pinguim "gigante" com bico pequeno
novo_dado <- data.frame(
  species = "Adelie", island = "Torgersen",
  bill_length_mm = 10,     # Valor muito baixo
  bill_depth_mm = 15,
  flipper_length_mm = 180,
  body_mass_g = 10000,     # Valor muito alto (10kg)
  sex = "male", year = 2007
)

penguins_mod <- rbind(penguins_data, novo_dado)

# Ajuste do novo modelo
modelo_mod <- lm(bill_length_mm ~ body_mass_g, data = penguins_mod)

# Comparação Visual
plot(penguins_mod$body_mass_g, penguins_mod$bill_length_mm, pch=19, col="gray",
     main = "Comparação com Outlier")
points(10000, 10, col="red", pch=19, cex=2) # Destacando o outlier
abline(modelo, col="blue", lwd=2, lty=2)      # Reta original
abline(modelo_mod, col="red", lwd=2)          # Nova reta

# Comparando metricas
rmse_mod <- sqrt(mean(residuals(modelo_mod)^2))
r2_mod <- summary(modelo_mod)$r.squared

print(paste("RMSE Modificado:", round(rmse_mod, 4)))
print(paste("R2 Modificado:", round(r2_mod, 4)))

# Discussão: Observe como a reta vermelha (com outlier) se inclina
# para tentar acomodar o ponto extremo, isso piora o ajuste para os dados reais.


# Augusto Oliveira Codo de Sousa - RM562080
# Felipe de Oliveira Cabral - RM561720
# Sofia Bueris de Souza - RM565818


# -------------------------------
# Instalar as bibliotecas 
# -------------------------------
install.packages("openxlsx")
install.packages("ggplot2")

library(openxlsx)
library(ggplot2)

# -------------------------------
# Ler o arquivo Excel (ajuste o caminho conforme seu arquivo)
# -------------------------------
dados <- read.xlsx("flood_risk_cleaned.xlsx")

# -------------------------------
# TABELA DE FREQUÊNCIA - VARIÁVEL DISCRETA (historical_floods)
# -------------------------------
freq_discreta <- table(dados$historical_floods)
df_freq_discreta <- as.data.frame(freq_discreta)

print(df_freq_discreta)  # mostrar no terminal
write.xlsx(df_freq_discreta, "freq_discreta_historical_floods.xlsx")  # salvar Excel

# -------------------------------
# TABELA DE FREQUÊNCIA - VARIÁVEL CONTÍNUA (rainfall_mm)
# -------------------------------
classes <- cut(dados$rainfall_mm, breaks = 6, right = FALSE)
freq_continua <- table(classes)
df_freq_continua <- as.data.frame(freq_continua)

print(df_freq_continua)  # mostrar no terminal
write.xlsx(df_freq_continua, "freq_continua_rainfall_mm.xlsx")  # salvar Excel

# -------------------------------
# GRÁFICO DE BARRAS - variável flood_occurred
# -------------------------------
graf_barras <- ggplot(dados, aes(x = as.factor(flood_occurred))) +
  geom_bar(fill = "lightblue") +
  labs(
    title = "Ocorrência de Enchentes",
    x = "Enchente Ocorrida (0 = Não, 1 = Sim)",
    y = "Frequência"
  ) +
  theme_minimal()

print(graf_barras)  # mostrar gráfico na aba plots
ggsave("grafico_barras_flood_occurred.png", plot = graf_barras, width = 7, height = 5)  # salvar PNG

# -------------------------------
# HISTOGRAMA - variável rainfall_mm
# -------------------------------
graf_histograma <- ggplot(dados, aes(x = rainfall_mm)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Distribuição da Chuva (mm)",
    x = "Chuva (mm)",
    y = "Frequência"
  ) +
  theme_minimal()

print(graf_histograma)  # mostrar gráfico na aba plots
ggsave("histograma_rainfall_mm.png", plot = graf_histograma, width = 7, height = 5)  # salvar PNG

# -------------------------------
# CÁLCULO DE MEDIDAS ESTATÍSTICAS - variável rainfall_mm
# -------------------------------
chuva <- dados$rainfall_mm

# Função para calcular moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Tendência central
media <- mean(chuva, na.rm = TRUE)
mediana <- median(chuva, na.rm = TRUE)
moda_valor <- moda(chuva)

# Dispersão
minimo <- min(chuva, na.rm = TRUE)
maximo <- max(chuva, na.rm = TRUE)
amplitude <- maximo - minimo
variancia <- var(chuva, na.rm = TRUE)
desvio_padrao <- sd(chuva, na.rm = TRUE)
coef_var <- (desvio_padrao / media) * 100

# Separatrizes (quartis)
quartis <- quantile(chuva, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Criar data.frame para salvar medidas
medidas <- data.frame(
  Medida = c("Média", "Mediana", "Moda", "Mínimo", "Máximo", "Amplitude",
             "Variância", "Desvio Padrão", "Coeficiente de Variação (%)",
             "1º Quartil (Q1)", "2º Quartil (Mediana)", "3º Quartil (Q3)"),
  Valor = c(media, mediana, moda_valor, minimo, maximo, amplitude,
            variancia, desvio_padrao, coef_var,
            quartis[1], quartis[2], quartis[3])
)

# Salvar medidas em Excel
write.xlsx(medidas, "medidas_estatisticas_rainfall_mm.xlsx")

# Formatar os valores para melhor visualização
medidas_formatadas <- medidas
medidas_formatadas$Valor <- formatC(medidas$Valor, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")

# Imprimir no console sem os números das linhas
print(medidas_formatadas, row.names = FALSE)


# -------------------------------
# INTERPRETAÇÃO - PARA RELATÓRIO
# -------------------------------
# Texto com placeholders
interpretacao <- "
Análise Estatística da variável 'rainfall_mm' (Chuva em mm):

- A média da chuva é de aproximadamente {media} mm, indicando o valor médio observado no conjunto de dados.
- A mediana, que é o valor central, está em {mediana} mm, mostrando que metade das observações está abaixo desse valor.
- A moda é {moda_valor} mm, representando o valor que ocorre com maior frequência.
- A amplitude da chuva varia de {minimo} mm até {maximo} mm, indicando uma variação total de {amplitude} mm.
- A variância e o desvio padrão ({variancia} e {desvio_padrao}, respectivamente) indicam a dispersão dos valores em torno da média.
- O coeficiente de variação é de {coef_var}%, mostrando a variabilidade relativa da chuva em relação à média.
- Os quartis indicam a divisão dos dados em quatro partes, sendo Q1: {q1} mm, Mediana (Q2): {q2} mm e Q3: {q3} mm.

Esses resultados são fundamentais para entender o comportamento da chuva na região estudada, podendo auxiliar na previsão e monitoramento de enchentes urbanas.
"

# Substituir placeholders pelos valores reais arredondados, escapando as chaves
interpretacao <- gsub("\\{media\\}", round(media, 2), interpretacao)
interpretacao <- gsub("\\{mediana\\}", round(mediana, 2), interpretacao)
interpretacao <- gsub("\\{moda_valor\\}", round(moda_valor, 2), interpretacao)
interpretacao <- gsub("\\{minimo\\}", round(minimo, 2), interpretacao)
interpretacao <- gsub("\\{maximo\\}", round(maximo, 2), interpretacao)
interpretacao <- gsub("\\{amplitude\\}", round(amplitude, 2), interpretacao)
interpretacao <- gsub("\\{variancia\\}", round(variancia, 2), interpretacao)
interpretacao <- gsub("\\{desvio_padrao\\}", round(desvio_padrao, 2), interpretacao)
interpretacao <- gsub("\\{coef_var\\}", round(coef_var, 2), interpretacao)
interpretacao <- gsub("\\{q1\\}", round(quartis[1], 2), interpretacao)
interpretacao <- gsub("\\{q2\\}", round(quartis[2], 2), interpretacao)
interpretacao <- gsub("\\{q3\\}", round(quartis[3], 2), interpretacao)

# Imprimir interpretação completa no console
cat(interpretacao)




# Przekształcone dane z tabeli – stosujemy środki przedziałów określonych w danej tabeli
# Dochody gospodarstw domowych (x)
incomes <- c(2500, 7500, 15000, 35000, 75000, 120000)

# Wartości portfela inwestycyjnego (y)
invest_portfolio_values <- c(5000, 40000, 75000, 300000, 625000, 1500000)

# Macierz liczebności osób
people_counts_mx <- matrix(c(
  46, 86, 38, 4, 0, 0,
  47, 99, 83, 19, 0, 0,
  17, 50, 67, 13, 0, 0,
  19, 62, 81, 26, 4, 0,
  3, 7, 26, 20, 4, 0,
  2, 2, 8, 10, 9, 2
), nrow = 6, byrow = TRUE)

data <- data.frame()
for (i in 1:6) {
  for (j in 1:6) {
    data <- rbind(data, data.frame(
      income = incomes[i],
      invest_portfolio_value = invest_portfolio_values[j],
      people_count = people_counts_mx[i, j]
    ))
  }
}

# Filtrowanie obserwacji (bez zer)
data <- subset(data, people_count > 0)

model <- lm(invest_portfolio_value ~ income, data = data, weights = people_count)

# Wyniki modelu
summary(model)

# Przedziały ufności dla parametrów
confint(model)

# Obliczanie środka ciężkości danych
income_mid  <- sum(data$income * data$people_count) / sum(data$people_count)
invest_portolio_value_mid <- sum(data$invest_portfolio_value * data$people_count) / sum(data$people_count)

# Wykres zależności
plot(data$income, data$invest_portfolio_value, 
     cex = sqrt(data$people_count)/3, 
     xlab = "Dochód gospodarstwa domowego", 
     ylab = "Wartość portfela inwestycyjnego", 
     main = "Zależność wartości portfela inwestycyjnego\nod dochodu gospodarstwa",
     pch = 1, col = "darkblue")

# Krzywa regresji
abline(model, col = "red", lwd = 2)

# Środek danych
points(income_mid, invest_portolio_value_mid,
       col = "darkgreen", bg = "green", pch = 21, cex = 2)

legend("topleft",
       legend = c("Obserwacje", "Prosta regresji", "Środek danych"),
       col = c("darkblue", "red", "darkgreen"),
       pch = c(1, NA, 21),
       pt.bg = c(NA, NA, "green"),
       lty = c(NA, 1, NA),
       pt.cex = c(1.2, NA, 2),
       lwd = c(NA, 2, NA))
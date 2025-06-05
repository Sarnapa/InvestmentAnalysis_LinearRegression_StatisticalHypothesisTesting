require(tidyverse)
require(MASS)
require(sn)

read_data <- function(csv_file) {
  data <- read_csv(csv_file, col_types = cols(.default = "d", Data = "D")) %>%
    dplyr::select(Month = Data, Open = Otwarcie, Close = Zamkniecie) %>%
    mutate(Month = format(Month, "%Y-%m")) %>%
    arrange(Month)
}

# Oblicz stopy zwrotu dla wszystkich możliwych startów inwestycji, dostarczając
# jako parametr długość inwestycji w miesiącach. 
# Założenie - kupno i sprzedaż na początku miesiąca.
calculate_return_rates <- function(df, months_count) {
  df %>%
    dplyr::select(Start_Investment_Month = Month, Open = Open) %>%
    mutate(Close = lead(Open, months_count), Return_Rate = (lead(Open, months_count) / Open) - 1) %>%
    filter(!is.na(Close))
}

compare_distribution <- function(returns_rates, distribution = "normal") {
  require(MASS)
  require(sn)
  
  # Histogram
  hist(returns_rates, breaks = 30, probability = TRUE,
       main = paste("Histogram 6 miesięcznych stóp zwrotu dla akcji KGHM Polska Miedź SA
                    w okresie styczeń 2015 - grudzień 2024"),
       xlab = "6 miesięczne stopy zwrotu", ylab = "Gęstość", 
       col = "lightblue", border = "blue", cex.main = 0.9)
  
  distribution_color = "orange"
  
  # Dopasowanie i rysowanie konkretnego rozkładu
  if (distribution == "normal") {
    mu <- mean(returns_rates)
    sigma <- sd(returns_rates)
    curve(dnorm(x, mean = mu, sd = sigma), col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu normalnego", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "t") {
    fit <- fitdistr(returns_rates, densfun = "t")$estimate
    curve(dt((x - fit["m"]) / fit["s"], df = fit["df"]) / fit["s"], 
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu T - Studenta", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "lognormal") {
    offset <- abs(min(returns_rates)) + 0.01
    returns_pos <- returns_rates + offset
    fit <- fitdistr(returns_pos, densfun = "log-normal")$estimate
    curve(dlnorm(x + offset, meanlog = fit["meanlog"], sdlog = fit["sdlog"]),
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu log - normalnego", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "skewnormal") {
    fit <- selm(returns_rates ~ 1, family = "SN")
    dp <- coef(fit, "dp")
    curve(dsn(x, xi = dp["xi"], omega = dp["omega"], alpha = dp["alpha"]),
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu pochylonego\nnormalnego", col = distribution_color, lwd = 2)
    
  } 
  else {
    stop("Obsługiwane rozkłady: 'normal', 't', 'lognormal', 'skewnormal'")
  }
}


kgh_df <- read_data("kgh_m.csv")

# Obliczenie 6 miesięcznych stóp zwrotu dla podanego instrumentu
kgh_6months_return_rates_df <- calculate_return_rates(kgh_df, 6)

# Porównanie histogramu z wybranym rozkładem
compare_distribution(kgh_6months_return_rates_df$Return_Rate, "normal")
compare_distribution(kgh_6months_return_rates_df$Return_Rate, "t")
compare_distribution(kgh_6months_return_rates_df$Return_Rate, "lognormal")
compare_distribution(kgh_6months_return_rates_df$Return_Rate, "skewnormal")

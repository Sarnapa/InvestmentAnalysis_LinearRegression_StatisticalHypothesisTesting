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

compare_distribution <- function(return_rates, distribution = "normal") {
  require(MASS)
  require(sn)
  
  # Histogram
  hist(return_rates, breaks = 30, probability = TRUE,
       main = "Histogram próby określającej 6-miesięczne stopy zwrotu 
       dla akcji KGHM Polska Miedź SA
       w okresie 01.2015 - 12.2024",
       xlab = "6-miesięczne stopy zwrotu", ylab = "Gęstość", 
       col = "lightblue", border = "blue", cex.main = 0.8)
  
  distribution_color = "red"
  
  # Dopasowanie i rysowanie konkretnego rozkładu
  if (distribution == "normal") {
    mu <- mean(return_rates)
    sigma <- sd(return_rates)
    curve(dnorm(x, mean = mu, sd = sigma), col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu normalnego", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "t") {
    fit <- fitdistr(return_rates, densfun = "t")$estimate
    curve(dt((x - fit["m"]) / fit["s"], df = fit["df"]) / fit["s"], 
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu T - Studenta", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "lognormal") {
    offset <- abs(min(return_rates)) + 0.01
    return_rates_pos <- return_rates + offset
    fit <- fitdistr(return_rates_pos, densfun = "log-normal")$estimate
    curve(dlnorm(x + offset, meanlog = fit["meanlog"], sdlog = fit["sdlog"]),
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu log - normalnego", col = distribution_color, lwd = 2)
    
  } 
  else if (distribution == "skewnormal") {
    fit <- selm(return_rates ~ 1, family = "SN")
    dp <- coef(fit, "dp")
    curve(dsn(x, xi = dp["xi"], omega = dp["omega"], alpha = dp["alpha"]),
          col = distribution_color, lwd = 2, add = TRUE)
    legend("topright", legend = "FGP rozkładu pochylonego\nnormalnego", col = distribution_color, lwd = 2)
    
  } 
  else {
    stop("Obsługiwane rozkłady: 'normal', 't', 'lognormal', 'skewnormal'")
  }
}

compare_cdf <- function(return_rates, distribution = "normal") {
  ecdf_color = "blue"
  cdf_color = "red"
  plot(ecdf(return_rates), main = "Porównanie dystrybuanty z próby określającej 6-miesięczne stopy zwrotu
    dla akcji KGHM Polska Miedź SA w okresie 01.2015 - 12.2024
    z dystrybuantą wybranego rozkładu",
    xlab = "6-miesięczna stopa zwrotu", ylab = "Dystrybuanta",
    col = ecdf_color, verticals = TRUE, do.points = FALSE, lwd = 2, cex.main = 0.8)
  
  x_vals <- seq(min(return_rates), max(return_rates), length.out = 500)
  
  if (distribution == "normal") {
    mu <- mean(return_rates)
    sigma <- sd(return_rates)
    lines(x_vals, pnorm(x_vals, mean = mu, sd = sigma), col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nnormalnego"),
    col = c(ecdf_color, cdf_color), lwd = 2)
    
  } else if (distribution == "t") {
    fit <- fitdistr(return_rates, "t", 
                    start = list(m = mean(return_rates), s = sd(return_rates), df = 5),
                    lower = c(-Inf, 0.001, 2))$estimate
    lines(x_vals, pt((x_vals - fit["m"]) / fit["s"], df = fit["df"]), col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nT - Studenta"), 
    col = c(ecdf_color, cdf_color), lwd = 2)
    
  } else if (distribution == "lognormal") {
    offset <- abs(min(return_rates)) + 0.01
    return_rates_pos <- return_rates + offset
    fit <- fitdistr(return_rates_pos, densfun = "log-normal")$estimate
    lines(x_vals, plnorm(x_vals + offset, meanlog = fit["meanlog"], sdlog = fit["sdlog"]),
          col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nlog - normalnego"),
    col = c(ecdf_color, cdf_color), lwd = 2)
    
  } else if (distribution == "skewnormal") {
    fit <- selm(return_rates ~ 1, family = "SN")
    dp <- coef(fit, "dp")
    lines(x_vals, psn(x_vals, xi = dp["xi"], omega = dp["omega"], alpha = dp["alpha"]),
          col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\npochylonego normalnego"),
    col = c(ecdf_color, cdf_color), lwd = 2)
    
  } else {
    stop("Obsługiwane rozkłady: 'normal', 't', 'lognormal', 'skewnormal'")
  }
}

kgh_df <- read_data("kgh_m.csv")

# Obliczenie 6 miesięcznych stóp zwrotu dla podanego instrumentu
kgh_6months_return_rates_df <- calculate_return_rates(kgh_df, 6)

# Porównanie histogramu z wybranym rozkładem
# Zakomentowane są rozkłady, które były analizowane, ale wybór padł na rozkład T - Studenta
compare_distribution(kgh_6months_return_rates_df$Return_Rate, "t")
#compare_distribution(kgh_6months_return_rates_df$Return_Rate, "normal")
#compare_distribution(kgh_6months_return_rates_df$Return_Rate, "lognormal")
#compare_distribution(kgh_6months_return_rates_df$Return_Rate, "skewnormal")

# Porównanie dystrybuanty empirycznej z próby z dystrybuaną wybranego rozkładu
# Zakomentowane są rozkłady, które były analizowane, ale wybór padł na rozkład T - Studenta
compare_cdf(kgh_6months_return_rates_df$Return_Rate, "t")
#compare_cdf(kgh_6months_return_rates_df$Return_Rate, "normal")
#compare_cdf(kgh_6months_return_rates_df$Return_Rate, "lognormal")
#compare_cdf(kgh_6months_return_rates_df$Return_Rate, "skewnormal")

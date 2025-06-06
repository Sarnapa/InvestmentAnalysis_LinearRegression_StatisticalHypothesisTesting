require(tidyverse)
require(MASS) # dla fitdistr (MLE)

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

plot_hist <- function(hist_data) {
  plot(hist_data, freq = FALSE,
       main = "Histogram próby określającej 6-miesięczne stopy zwrotu 
       dla akcji KGHM Polska Miedź SA
       w okresie 01.2015 - 12.2024",
       xlab = "6-miesięczne stopy zwrotu", ylab = "Gęstość", 
       col = "lightblue", border = "blue", cex.main = 0.8)
}

# Estymacja MLE dla rozkładu normalnego
# Parametry: wartość oczekiwana, odchylenie standardowe
# Metoda: metoda maksymalnego prawdopodobieństwa (MLE)
fit_normal <- function(data) {
  fit <- fitdistr(data, "normal")
  estimated_params <- fit$estimate
  error_sd <- fit$sd
  
  cat("Rozkład normalny:\n")
  cat("Wartość oczekiwana:", estimated_params["mean"], "(Odchylenie standardowe estymacji:", error_sd["mean"], ")\n")
  cat("Odchylenie standardowe:", estimated_params["sd"], "(Odchylenie standardowe estymacji:", error_sd["sd"], ")\n\n")
  
  return(fit)
}

# Estymacja MLE dla rozkładu T-Studenta (położenie, skala, stopnie swobody)
# Parametry: m (położenie), sd (skala), df (stopnie swobody)
# Metoda: metoda maksymalnego prawdopodobieństwa (MLE)
fit_t_student <- function(data, start_degress_of_freedom) {
  fit <- fitdistr(data, densfun = "t", 
                  start = list(m = mean(data), s = sd(data), df = start_degress_of_freedom),
                  lower = c(-Inf, 0.001, 2))  # dla bezpieczeństwa
  estimated_params <- fit$estimate
  error_sd <- fit$sd
  
  cat("Rozkład T-Studenta:\n")
  cat("Położenie:", estimated_params["m"], "(Odchylenie standardowe estymacji:", error_sd["m"], ")\n")
  cat("Skala:", estimated_params["s"], "(Odchylenie standardowe estymacji:", error_sd["s"], ")\n")
  cat("Stopnie swobody:", estimated_params["df"], "(Odchylenie standardowe estymacji:", error_sd["df"], ")\n\n")
  
  return(fit)
}

# Estymacja MLE dla rozkładu log - normalnego po przesunięciu danych, aby były > 0
# Parametry: log wartość oczekiwana, log odchylenie standardowe
# Metoda: metoda maksymalnego prawdopodobieństwa (MLE)
fit_log_normal <- function(data, offset) {
  shifted_data <- data + offset
  
  fit <- fitdistr(shifted_data, "log-normal")
  estimated_params <- fit$estimate
  error_sd <- fit$sd
  
  cat("Rozkład log-normalny (z przesunięciem o", offset, "):\n")
  cat("log wartość oczekiwana:", estimated_params["meanlog"], "(Odchylenie standardowe estymacji:", error_sd["meanlog"], ")\n")
  cat("log odchylenie standardowe:", estimated_params["sdlog"], "(Odchylenie standardowe estymacji:", error_sd["sdlog"], ")\n\n")
  
  return(fit)
}

compare_distribution <- function(hist_data, fit_object, distribution = "normal", offset = 0) {
  distribution_color = "red"  
  
  # Ekstrakcja granic wykresu
  x_min <- min(hist_data$breaks)
  x_max <- max(hist_data$breaks)
  x_vals <- seq(x_min, x_max, length.out = 500)
  
  plot_hist(hist_data)

  if (distribution == "normal") {
    mean <- fit_object$estimate["mean"]
    sd <- fit_object$estimate["sd"]
    lines(x_vals, dnorm(x_vals, mean = mean, sd = sd), col = distribution_color, lwd = 2)
    legend("topright", legend = "FGP rozkładu normalnego", col = distribution_color, lwd = 2)
  }
  else if (distribution == "t") {
    m <- fit_object$estimate["m"]
    s <- fit_object$estimate["s"]
    df <- fit_object$estimate["df"]
    lines(x_vals, dt((x_vals - m) / s, df) / s, col = distribution_color, lwd = 2)
    legend("topright", legend = "FGP rozkładu T - Studenta", col = distribution_color, lwd = 2)
  }
  else if (distribution == "lognormal") {
    meanlog <- fit_object$estimate["meanlog"]
    sdlog <- fit_object$estimate["sdlog"]
    lines(x_vals, dlnorm(x_vals + offset, meanlog = meanlog, sdlog = sdlog), col = distribution_color, lwd = 2)
    legend("topright", legend = "FGP rozkładu\nlog - normalnego", col = distribution_color, lwd = 2)
  }
  else {
    stop("Obsługiwane rozkłady: 'normal', 't', 'lognormal'")
  }
}

compare_cdf <- function(data, fit_object, distribution = "normal", offset = 0) {
  ecdf_color = "blue"
  cdf_color = "red"
  
  # Ekstrakcja granic wykresu
  x_min <- min(data)
  x_max <- max(data)
  x_vals <- seq(x_min, x_max, length.out = 500)
  
  plot(ecdf(data), main = "Porównanie dystrybuanty z próby określającej 6-miesięczne stopy zwrotu
    dla akcji KGHM Polska Miedź SA w okresie 01.2015 - 12.2024
    z dystrybuantą wybranego rozkładu",
    xlab = "6-miesięczna stopa zwrotu", ylab = "Dystrybuanta",
    col = ecdf_color, verticals = TRUE, do.points = FALSE, lwd = 2, cex.main = 0.8, , yaxt = "n")
  
  # Dodajemy poziome linie pomocnicze:
  abline(h = seq(0, 1, by = 0.2), col = "gray80", lty = "dashed")
  
  # Własna oś Y z etykietami co 0.2
  axis(side = 2, at = seq(0, 1, by = 0.2), las = 1)
  
  if (distribution == "normal") {
    mean <- fit_object$estimate["mean"]
    sd <- fit_object$estimate["sd"]
    lines(x_vals, pnorm(x_vals, mean = mean, sd = sd), col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nnormalnego"),
    col = c(ecdf_color, cdf_color), lwd = 2)
  } 
  else if (distribution == "t") {
    m <- fit_object$estimate["m"]
    s <- fit_object$estimate["s"]
    df <- fit_object$estimate["df"]
    lines(x_vals, pt((x_vals - m) / s, df), col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nT - Studenta"),
    col = c(ecdf_color, cdf_color), lwd = 2)
  } 
  else if (distribution == "lognormal") {
    meanlog <- fit_object$estimate["meanlog"]
    sdlog <- fit_object$estimate["sdlog"]
    lines(x_vals, plnorm(x_vals + offset, meanlog = meanlog, sdlog = sdlog),
          col = cdf_color, lwd = 2)
    legend("bottomright", legend = c("Dystrybuanta z próby", "Dystrybuanta rozkładu\nlog - normalnego"),
    col = c(ecdf_color, cdf_color), lwd = 2)
  }
  else {
    stop("Obsługiwane rozkłady: 'normal', 't', 'lognormal'")
  }
}

kgh_df <- read_data("kgh_m.csv")

# Obliczenie 6 miesięcznych stóp zwrotu dla podanego instrumentu
kgh_6months_return_rates_df <- calculate_return_rates(kgh_df, 6)

# Wyznaczenie i narysowanie histogramu
hist_data <- hist(kgh_6months_return_rates_df$Return_Rate, breaks = 30, plot = FALSE)
plot_hist(hist_data)

# Dopasowanie do różnych rozkładów
# Zakomentowane są rozkłady, które były analizowane, ale wybór padł na rozkład T - Studenta
# Rozpoczynamy szukanie dopasowania od 3 stopni swobody.
fit_t_student_res <- fit_t_student(kgh_6months_return_rates_df$Return_Rate, 3)
#fit_normal_res <- fit_normal(kgh_6months_return_rates_df$Return_Rate)
## +0.01 żeby na pewno było większe od 0
#offset <- abs(min(kgh_6months_return_rates_df$Return_Rate)) + 0.01
#fit_log_normal_res <- fit_log_normal(kgh_6months_return_rates_df$Return_Rate, offset)

# Porównanie histogramu z wybranym rozkładem
compare_distribution(hist_data, fit_t_student_res, "t")
#compare_distribution(hist_data, fit_normal_res, "normal")
#compare_distribution(hist_data, fit_log_normal_res, "lognormal", offset)

# Porównanie dystrybuanty empirycznej z próby z dystrybuaną wybranego rozkładu
compare_cdf(kgh_6months_return_rates_df$Return_Rate, fit_t_student_res, "t")
#compare_cdf(kgh_6months_return_rates_df$Return_Rate, fit_normal_res, "normal")
#compare_cdf(kgh_6months_return_rates_df$Return_Rate, fit_log_normal_res, "lognormal", offset)

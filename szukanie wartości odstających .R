# Wczytanie bibliotek
library(ggplot2)
library(gridExtra)

# Wczytanie danych z pliku CSV
data <- read.csv("supermarket_filled.csv")

# Rysowanie boxplotów dla wybranych zmiennych
b1 <- ggplot(data, aes(y = Unit.price)) +
  geom_boxplot() +
  labs(title = "Boxplot: Unit Price") +
  theme_minimal()

b2 <- ggplot(data, aes(y = Total)) +
  geom_boxplot() +
  labs(title = "Boxplot: Total") +
  theme_minimal()

# Rysowanie histogramów dla wybranych zmiennych
h1 <- ggplot(data, aes(x = Unit.price)) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  labs(title = "Histogram: Unit Price", x = "Unit Price", y = "Frequency") +
  theme_minimal()

h2 <- ggplot(data, aes(x = Total)) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  labs(title = "Histogram: Total", x = "Total", y = "Frequency") +
  theme_minimal()

# Wyświetlenie wykresów w siatce 2x2
grid.arrange(b1, b2, h1, h2, nrow = 2)

replace_outliers_with_median <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- median(column, na.rm = TRUE)
  return(column)
}

# Analiza wartości odstających dla kolumn
numeric_columns <- c("Unit.price", "Total", "Quantity", "Rating")
outliers <- lapply(data[numeric_columns], detect_outliers)

# Wyświetlenie wartości odstających
outliers

# Zastąpienie wartości odstających w danych
data[numeric_columns] <- lapply(data[numeric_columns], replace_outliers_with_median)

# Sprawdzenie wartości odstających po zastąpieniu
outliers_after <- lapply(data[numeric_columns], detect_outliers)

# Wyświetlenie liczby wartości odstających po zastąpieniu
cat("\nLiczba wartości odstających po zastąpieniu:\n")
sapply(outliers_after, length)

# Porównanie przed i po w postaci tabeli
comparison <- data.frame(
  Variable = numeric_columns,
  Outliers_Before = sapply(outliers_before, length),
  Outliers_After = sapply(outliers_after, length)
)
print(comparison)

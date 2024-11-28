# Grupa-3-analiza-danych

dane <- read.csv(file="supermarket_new.csv", header = TRUE, sep = ",", dec = ".")

# Pakiety
library(naniar)
library(reshape2)
library(ggplot2)

# Liczba NA
n_miss(dane)

# Tabela podsumowująca liczbę NA
miss_var_summary(dane)

# Wizualizacja lokalizacji NA (Shadow map)
vis_miss(dane, sort = TRUE)

# Macierz korelacji braków
NA_matrix <- is.na(dane)
NA_cor <- cor(NA_matrix)
NA_data <- melt(NA_cor)
ggplot(NA_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#81b29a", high = "#bc4b51", mid = "#f4f1de", midpoint = 0) +
  theme_minimal() +
  labs(title = "Macierz korelacji braków", x = "Zmienne", y = "Zmienne")



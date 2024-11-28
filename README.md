# Grupa-3-analiza-danych

dane <- read.csv(file="supermarket_new.csv", header = TRUE, sep = ",", dec = ".")

library(naniar)

# Liczba NA
n_miss(dane)

# Tabela podsumowująca liczbę NA
miss_var_summary(dane)

# Wizualizacja lokalizacji NA (Shadow map)
vis_miss(dane, sort = TRUE)



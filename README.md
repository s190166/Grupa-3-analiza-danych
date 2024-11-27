# Grupa-3-analiza-danych

dane <- read.csv(file="supermarket_new (1).csv", header = TRUE, sep = ",", dec = ".")

# Liczba NA
n_miss(dane)

# Wizualizacja lokalizacji NA (Shadow map) 
vis_miss(dane)
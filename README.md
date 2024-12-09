# Grupa-3-analiza-danych
# Instalacja i załadowanie pakietu
install.packages("mice")
library(mice)

# Wczytanie danych z pliku CSV
data <- read.csv("C:/Users/zuzal/OneDrive/Desktop/analiza/supermarket_new.csv")

# Podgląd brakujących danych
md.pattern(data)

# Uzupełnianie brakujących wartości w kolumnie 'Rating'
imputed_data <- mice(data, m = 5, method = "pmm", maxit = 50, seed = 123)

# Wybór jednego zestawu danych uzupełnionych
completed_data <- complete(imputed_data, 1)

# Eksport danych do nowego pliku CSV
write.csv(completed_data, "supermarket_filled.csv", row.names = FALSE)

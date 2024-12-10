# Instalacja i załadowanie pakietów (jeśli nie są zainstalowane)
if (!require("dplyr")) install.packages("dplyr")
if (!require("mice")) install.packages("mice")
library(dplyr)
library(mice)

# Wczytaj dane
data <- supermarket

# Zmapuj zależność między Branch a City
branch_to_city <- data %>%
  filter(!is.na(City)) %>%
  distinct(Branch, City) %>%
  group_by(Branch) %>%
  summarize(City = first(City))

# Połącz dane, aby uzupełnić brakujące wartości
data <- data %>%
  left_join(branch_to_city, by = "Branch", suffix = c("", ".y")) %>%
  mutate(City = ifelse(is.na(City), City.y, City)) %>%
  select(-City.y)

# Sprawdź brakujące wartości w kolumnie City
sum(is.na(data$City))

# Zastąp wartości NA w 16 kolumnie wartościami z 9 kolumny
data <- data %>%
  mutate(`gross income` = ifelse(is.na(`gross income`), data[[9]], `gross income`))

# Sprawdź brakujące wartości w kolumnie 'gross income'
sum(is.na(data$`gross income`))

# Uzupełnianie brakujących wartości w całym zbiorze danych przy użyciu pakietu mice
# (upewnij się, że dane wejściowe nie zawierają nienumerowalnych kolumn)
imputed_data <- mice(data, m = 1, method = "pmm", maxit = 50, seed = 123)

# Wybór uzupełnionego zestawu danych
data <- complete(imputed_data, 1)

# Sprawdzenie liczby brakujących wartości w całym zbiorze danych
sum(is.na(data))

# Eksport danych do pliku CSV
write.csv(data, "supermarket_filled.csv", row.names = FALSE)

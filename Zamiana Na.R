# Wczytaj pakiety (jeśli nie są zainstalowane, zainstaluj je najpierw)
library(dplyr)

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
  mutate(across(16, ~ ifelse(is.na(.), data[[9]], .)))

# Sprawdź, czy są jeszcze braki w 16 kolumnie
sum(is.na(data[[16]]))


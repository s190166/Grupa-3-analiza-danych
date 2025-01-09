# Wczytanie bibliotek
library(ggplot2)
library(dplyr)

# Wczytanie danych z pliku CSV
data <- read.csv("supermarket_filled.csv")

# Podział danych według płci i metody płatności
gender_payment_counts <- data %>%
  group_by(Gender, Payment) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1)) %>%
  ungroup()

# Tworzenie wykresu kołowego dla kobiet
plot_female <- ggplot(
  gender_payment_counts %>% filter(Gender == "Female"),
  aes(x = "", y = Percentage, fill = Payment)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Metody płatności kobiet") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

# Tworzenie wykresu kołowego dla mężczyzn
plot_male <- ggplot(
  gender_payment_counts %>% filter(Gender == "Male"),
  aes(x = "", y = Percentage, fill = Payment)
) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Metody płatności mężczyzn") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

# Wyświetlenie wykresów obok siebie
library(gridExtra)
grid.arrange(plot_female, plot_male, ncol = 2)

# Tworzenie histogramu z podziałem na miasta
ggplot(data, aes(x = Rating)) +
  geom_histogram(
    binwidth = 0.5,  # Szerokość binów
    fill = "blue",   # Kolor wypełnienia
    color = "black"  # Kolor obramowania
  ) +
  facet_wrap(~ City) +  # Podział na miasta
  labs(
    title = "Rozkład ocen klientów w różnych miastach",
    x = "Ocena",
    y = "Liczba klientów"
  ) +
  theme_minimal()  # Estetyczny motyw


# Tworzenie wykresu słópkowego 
# Obliczanie średniej sprzedaży dla każdej linii produktów
average_sales <- aggregate(Total ~ Product.line, data = data, mean)

# Zwiększenie marginesów dla większej przestrzeni na etykiety
par(mar = c(8, 4, 4, 2) + 0.1)

# Tworzenie wykresu słupkowego z bardziej czytelnymi etykietami
barplot(
  average_sales$Total, 
  names.arg = average_sales$Product.line, 
  col = "lightblue",  # Kolor słupków
  main = "Średnia sprzedaż według linii produktów", 
  xlab = "",          # Usuń opis osi X, aby nie dublować etykiet
  ylab = "Średnia sprzedaż",
  las = 2,            # Pionowe etykiety osi X
  cex.names = 0.6     # Zmniejszenie rozmiaru tekstu etykiet
)

# Dodanie opisu osi X poniżej wykresu
mtext("Linia produktów", side = 1, line = 7, cex = 1.2)


# Tworzenie wykresu trendów 
# Konwersja kolumny 'Date' na format daty
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Grupowanie danych według daty i obliczanie całkowitej sprzedaży
daily_sales <- aggregate(Total ~ Date, data = data, sum)

# Tworzenie wykresu liniowego
plot(
  daily_sales$Date, 
  daily_sales$Total, 
  type = "l",             # Typ wykresu: linia
  col = "blue",           # Kolor linii
  lwd = 2,                # Grubość linii
  main = "Sprzedaż w czasie", 
  xlab = "Data", 
  ylab = "Całkowita sprzedaż"
)

# Dodanie punktów na wykresie dla lepszej widoczności
points(
  daily_sales$Date, 
  daily_sales$Total, 
  col = "black",            # Kolor punktów
  pch = 16                # Typ punktów: kropki
)
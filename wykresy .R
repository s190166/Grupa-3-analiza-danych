# Wczytanie bibliotek
library(ggplot2)

# Wykres kołowy
# Wczytanie danych z pliku CSV
data <- read.csv("supermarket_filled.csv")

# Obliczenie udziałów poszczególnych typów płatności
payment_counts <- table(data$Payment)
payment_percent <- round((payment_counts / sum(payment_counts)) * 100, 1)

# Przygotowanie danych dla ggplot
payment_data <- data.frame(
  Payment = names(payment_counts),
  Percentage = as.numeric(payment_percent)
)

# Tworzenie wykresu kołowego
ggplot(payment_data, aes(x = "", y = Percentage, fill = Payment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Usunięcie siatki i osi
  labs(title = "Udział typów płatności") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") # Dodanie estetycznej palety kolorów

# Tworzenie histogramu
ggplot(data, aes(x = Rating)) +
  geom_histogram(
    binwidth = 0.5,  # Szerokość binów
    fill = "blue",   # Kolor wypełnienia
    color = "black"  # Kolor obramowania
  ) +
  labs(
    title = "Rozkład ocen klientów",
    x = "Ocena",
    y = "Liczba klientów"
  ) +
  theme_minimal()  # Estetyczny motyw

# Tworzenie wykresu słópkowego 
# Obliczanie średniej sprzedaży dla każdej linii produktów
average_sales <- aggregate(Total ~ Product.line, data = data, mean)

# Tworzenie wykresu słupkowego
barplot(
  average_sales$Total, 
  names.arg = average_sales$Product.line, 
  col = "lightblue",  # Kolor słupków
  main = "Średnia sprzedaż według linii produktów", 
  xlab = "Linia produktów", 
  ylab = "Średnia sprzedaż",
  las = 2             # Obrót etykiet osi X (do pionowego wyświetlenia)
)

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
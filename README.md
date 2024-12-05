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

# Wykres UpSet dla współwystępowania NA
gg_miss_upset(dane, 
              nsets = 3)
  
# Przekodowanie zmiennych jakościowych na ilościowe
dane2 <- dane

row.names(dane2) <- dane$Invoice.ID
dane2$Invoice.ID <- NULL


dane2$Branch <- ifelse((dane2$Branch) == "A", 1,
                       ifelse(dane2$Branch == "B", 2, 0))

dane2$City <- ifelse(is.na(dane2$City), NA,
                    ifelse(dane2$City == "Naypyitaw", 1,
                       ifelse(dane2$City == "Mandalay", 2, 0)))

dane2$Customer.type <- ifelse((dane2$Customer.type) == "Member", 1, 0)
                          
dane2$Gender <- ifelse((dane2$Gender) == "Male", 1, 0)

dane2$Product.line <- ifelse(dane2$Product.line == "Electronic accessories", 1,
                             ifelse(dane2$Product.line == "Fashion accessories", 2,
                                    ifelse(dane2$Product.line == "Food and beverages", 3,
                                           ifelse(dane2$Product.line == "Health and beauty", 4,
                                                  ifelse(dane2$Product.line == "Home and lifestyle", 5, 0)))))

dane2$Payment <- ifelse(dane2$Payment == "Cash", 1,
                        ifelse(dane2$Payment == "Credit card", 2, 0))

# Macierz korelacji braków (dla zmiennych ilościowych)
NA_matrix <- is.na(dane2)
NA_cor <- cor(NA_matrix)
NA_data <- melt(NA_cor)
ggplot(NA_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#81b29a", high = "#bc4b51", mid = "#f4f1de", midpoint = 0) +
  theme_minimal() +
  labs(title = "Macierz korelacji braków", x = "Zmienne", y = "Zmienne")


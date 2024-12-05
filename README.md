# Grupa-3-analiza-danych - Zuzanna łomża

dane <- read.csv(file="supermarket_new.csv", header = TRUE, sep = ",", dec = ".")

# Pakiety
library(naniar)
library(reshape2)
library(ggplot2)
library(rstatix)
library(ggcorrplot)

# Liczba NA
n_miss(dane)

# Tabela podsumowująca liczbę NA
miss_var_summary(dane)

# Wizualizacja lokalizacji NA (Shadow map)
vis_miss(dane, sort = TRUE)

# Wykres UpSet dla współwystępowania NA
gg_miss_upset(dane, 
              nsets = 3)

# Mapa cieplna liczby NA dla kolumn z NA
gg_miss_fct(dane, fct = City)
gg_miss_fct(dane, fct = gross.income)
gg_miss_fct(dane, fct = Rating)

# Przekodowanie zmiennych jakościowych na ilościowe
dane2 <- data.frame(dane, row.names = TRUE)

dane2$Branch <- ifelse((dane2$Branch) == "A", 1,
                       ifelse(dane2$Branch == "B", 2, 0))

dane2$City <- ifelse(is.na(dane2$City), NA,
                    ifelse(dane2$City == "Naypyitaw", 1,
                       ifelse(dane2$City == "Mandalay", 2, 0)))

dane2$Customer.type <- ifelse((dane2$Customer.type) == "Member", 1, 0)
                          
dane2$Gender <- ifelse((dane2$Gender) == "Male", 1, 0)

dane2$Product.line <- ifelse(dane2$Product.line == "Electronic accessories", 1,
                             ifelse(dane2$Product.line == "Fashion accessories", 2,
                                    ifelse(dane2$Product.line == "Food and beverage", 2,
                                           ifelse(dane2$Product.line == "Health and beauty", 3,
                                                  ifelse(dane2$Product.line == "Health and lifestyle", 3, 0)))))

dane2$Payment <- ifelse(dane2$Payment == "Cash", 1,
                        ifelse(dane2$Payment == "Credit card", 2, 0))

dane2$Quantity <- as.numeric(dane2$Quantity)

dane2$Date <- as.Date(dane2$Date, format = "%m/%d/%Y")
dane2$Date <- as.numeric(format(dane2$Date, "%Y%m%d"))

dane2$Time <- as.numeric(sub(":(\\d{2}):.*", ".\\1", dane2$Time))

# Korelacja danych
NA_cor <- cor_mat(dane2)
dane2$gross.margin.percentage <- NULL # "gross.margin.percentage" posiada te same wartości, przez co niemożliwym jest policzenie dla nich korelacji
NA_cor <- cor_mat(dane2)

# Macierz korelacji braków
ggcorrplot(NA_cor)

# Wykres zależności Dochodu brutto i Oceny stratyfikacji klientów dotycząca ich ogólnego doświadczenia zakupowego
ggplot(data = dane, aes(x = gross.income, y = Rating)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()


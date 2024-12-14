# Grupa-3-analiza-danych - Zuzanna Łomża

data <- read.csv("supermarket_new.csv")

# Pakiety
library(naniar)
library(reshape2)
library(ggplot2)
library(rstatix)
library(ggcorrplot)
library(mice)
library(validate)

# Brakujące obserwacje
## Liczba NA
n_miss(data)

## Tabela podsumowująca liczbę NA
miss_var_summary(data)

## Wizualizacja lokalizacji NA (Shadow map)
vis_miss(data, sort = TRUE)

## Wykres UpSet dla współwystępowania NA
gg_miss_upset(data, 
              nsets = 3)

## Mapa cieplna liczby NA dla "Miasto" ("Oddział")
gg_miss_fct(data, fct = City)

## Przekodowanie zmiennych jakościowych na ilościowe
data2 <- data.frame(data, row.names = TRUE)

data2$Branch <- ifelse((data2$Branch) == "A", 1,
                       ifelse(data2$Branch == "B", 2, 0))

data2$City <- ifelse(is.na(data2$City), NA,
                    ifelse(data2$City == "Naypyitaw", 1,
                       ifelse(data2$City == "Mandalay", 2, 0)))

data2$Customer.type <- ifelse((data2$Customer.type) == "Member", 1, 0)
                          
data2$Gender <- ifelse((data2$Gender) == "Male", 1, 0)

data2$Product.line <- ifelse(data2$Product.line == "Electronic accessories", 1,
                             ifelse(data2$Product.line == "Fashion accessories", 2,
                                    ifelse(data2$Product.line == "Food and beverage", 2,
                                           ifelse(data2$Product.line == "Health and beauty", 3,
                                                  ifelse(data2$Product.line == "Health and lifestyle", 3, 0)))))

data2$Payment <- ifelse(data2$Payment == "Cash", 1,
                        ifelse(data2$Payment == "Credit card", 2, 0))

data2$Quantity <- as.numeric(data2$Quantity)

data2$Date <- as.Date(data2$Date, format = "%m/%d/%Y")
data2$Date <- as.numeric(format(data2$Date, "%d%m%Y"))

data2$Time <- as.numeric(sub(":(\\d{2}):.*", ".\\1", data2$Time))

## Korelacja braków
NA_cor <- cor_mat(data2)
#### Wartości gross.merge.percentage powtażają się w każdym wierszu, przez co niemożliwym jest policzenie korelacji dla tej zmiennej

## Wykluczenie zmiennej "gross.merge.percentage"
data_cor <- data2 %>%
            mutate(gross.margin.percentage = NULL)

NA_cor2 <- cor_mat(data_cor)

## Macierz korelacji braków
ggcorrplot(NA_cor2)

## Mapa NA według wierszy
md.pattern(data2, rotate.names = TRUE)

## Wykres zależności "Dochodu brutto" i "Opłaty podatkowej (5%)"
ggplot(data = data2, aes(x = gross.income, y = Tax.5.)) + 
  geom_point() +
  geom_miss_point() +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal()
  

# Walidacja danych
## Dodawanie zmiennych pomocniczych
check_cogs <- data$Quantity * data$Unit.price %>%
              as.data.frame()
check_Tax.5. <- .05 * data$cogs %>%
                as.data.frame()
check_Total <- data$Tax.5. + data$cogs %>%
               as.data.frame()

## Sprawdzanie poprawności danych według reguł
check_that(data,
           if (City == "Yangon") Branch == "A",
           if (City == "Mandalay") Branch == "B",
           if (City == "Naypyitaw") Branch == "C",
           Unit.price >= 0,
           Quantity >= 0,
           Tax.5. == check_Tax.5.,
           Total == check_Total,
           cogs == check_cogs,
           gross.margin.percentage >= 0,
           gross.income == Tax.5.,
           Rating >= 1,
           Rating <= 10)

## Definowanie reguł
rules <- validator(if (City == "Yangon") Branch == "A",
                   if (City == "Mandalay") Branch == "B",
                   if (City == "Naypyitaw") Branch == "C",
                   Unit.price >= 0,
                   Quantity >= 0,
                   Tax.5. == check_Tax.5.,
                   Total == check_Total,
                   cogs == check_cogs,
                   gross.margin.percentage >= 0,
                   gross.income == Tax.5.,
                   Rating >= 1,
                   Rating <= 10)

## Wizualizacja braków oraz błędów danych według reguł
cf <- confront(data, rules) %>%
      barplot(main = "Wyniki walidacji danych według reguł")

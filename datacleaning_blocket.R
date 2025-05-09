# Installera och ladda n�dv�ndiga paket
install.packages(c("tidyverse", "ggplot2", "caret"))
library(tidyverse)
library(ggplot2)
library(caret)

# L�s in CSV-filen
data <- read.csv("C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/datainsamling_blocket_volkswagen.csv", fileEncoding = "UTF-8")


#CLEANING

# Kolla p� datastrukturen
str(data)
head(data)
summary(data)

# Identifiera alla kolumner som �r av typen character
character_cols <- names(data)[sapply(data, is.character)]

# Loopa genom dessa kolumner och konvertera till gemener
for (col in character_cols) {
  data[[col]] <- tolower(data[[col]])
}


# R�kna NA-v�rden per kolumn
na_counts <- colSums(is.na(data))
print("Antal NA-v�rden per kolumn:")
print(na_counts)
# 3 st i horespower

# R�kna tomma textstr�ngar per kolumn
empty_string_counts <- sapply(data, function(x) sum(x == ""))
print("\nAntal tomma textstr�ngar per kolumn:")
print(empty_string_counts)
# 1 st fuel_type, 5 st drive_whell_config, 3 st color, 7 st registration_date, 16 st

data$fuel_type[data$fuel_type == ""] <- NA
data$drive_wheel_config[data$drive_wheel_config == ""] <- NA
data$color[data$color == ""] <- NA
data$registration_date[data$registration_date == ""] <- NA

# Kontrollera resultatet
na_counts_updated <- colSums(is.na(data))
print("Antal NA-v�rden per kolumn efter konvertering av tomma str�ngar:")
print(na_counts_updated)

# Konvertera 'registration_date' till Date-format
data$registration_date <- as.Date(data$registration_date)


# Identifiera alla kolumner som �r av typen character
character_cols <- names(data)[sapply(data, is.character)]

# Skriv ut de unika v�rdena f�r varje character-kolumn
for (col in character_cols) {
  cat(paste("Unika v�rden i kolumnen '", col, "':\n", sep = ""))
  print(unique(data[[col]]))
  cat("\n")
}

# �tg�rda inkonsekvenser i 'fuel_type'
data$fuel_type[data$fuel_type == "milj�br�nsle"] <- "milj�br�nsle/hybrid"
data$fuel_type[data$fuel_type == "disel"] <- "diesel"

# �tg�rda inkonsekvenser i 'body_type'
data$body_type[data$body_type == "halvkomni"] <- "halvkombi"

# �tg�rda inkonsekvenser i 'color'
data$color[data$color == "siver"] <- "silver"
data$color[data$color == "v�t"] <- "vit"
data$color[data$color == "m�rbll�"] <- "m�rkbl�"

# �tg�rda inkonsekvenser i 'model'
data$model[data$model == "up"] <- "up!"
data$model[data$model == "turan"] <- "touran"

# Hantera felaktiga v�rden i 'model'
data$model[data$model %in% c("vit", "svart", "silver")] <- NA

# Kontrollera resultatet
for (col in character_cols) {
  cat(paste("Unika v�rden i kolumnen '", col, "':\n", sep = ""))
  print(unique(data[[col]]))
  cat("\n")
}

# Trimma eventuella inledande eller avslutande mellanslag
data$color <- trimws(data$color)

# Ta bort prefixet "ljus"
data$color <- gsub("^ljus", "", data$color)

# Ta bort prefixet "m�rk"
data$color <- gsub("^m�rk", "", data$color)

# Trimma igen f�r att ta bort eventuella nya inledande mellanslag som kan ha uppst�tt
# om "ljus" eller "m�rk" togs bort och det fanns ett mellanslag efter
data$color <- trimws(data$color)

# Kontrollera de unika f�rgerna igen
cat("Unika v�rden i kolumnen 'color' efter standardisering:\n")
print(unique(data$model))
cat("\n")

# R�kna antalet f�rekomster av varje unik modell
model_counts <- table(data$model)

# Skriv ut antalet f�r varje modell p� en ny rad
print(model_counts)

# Standardisera 'model'-kolumnen
data$model[data$model == "new beetle"] <- "beetle"
data$model[data$model == "crosstouran"] <- "touran"
data$model[data$model == "eos"] <- NA
data$model[data$model == "gti"] <- NA
data$model[data$model == "jetta"] <- NA
data$model[data$model == "lupo"] <- NA
data$model[data$model == "phaeton"] <- NA
data$model[data$model == "polo cross"] <- "polo"
data$model[data$model == "tiguan allspace"] <- "tiguan"


# R�kna antalet rader som har minst ett NA-v�rde
na_rows_count <- sum(!complete.cases(data))
cat("Antal rader med minst ett NA-v�rde:", na_rows_count, "\n")

# 10 dyraste bilarna
top_10_price <- data %>%
  arrange(desc(selling_price)) %>%
  head(10)
cat("De 10 dyraste bilarna:\n")
print(top_10_price)
cat("\n")

# 10 bilarna med h�gst miltal
top_10_mileage <- data %>%
  arrange(desc(mileage)) %>%
  head(10)
cat("De 10 bilarna med h�gst miltal:\n")
print(top_10_mileage)
cat("\n")

# 10 bilarna med h�gst modell�r
top_10_model_year <- data %>%
  arrange(desc(model_year)) %>%
  head(10)
cat("De 10 bilarna med h�gst modell�r:\n")
print(top_10_model_year)

# Identifiera och s�tta NA f�r den dyra rallybilen (pris 2650000)
data$selling_price[data$selling_price == 2650000] <- NA

# Identifiera och s�tta NA f�r bilen med orimligt h�gt miltal (miltal 249764)
data$mileage[data$mileage == 249764] <- NA

# Identifiera och s�tta NA f�r bilen med orimligt h�gt modell�r (modell�r 2025)
data$model_year[data$model_year == 2025] <- NA


# Ta bort alla rader med minst ett NA-v�rde
data_cleaned <- data[complete.cases(data), ]

# Kontrollera antalet rader efter borttagning
cat("Antal rader i den ursprungliga datan:", nrow(data), "\n")
cat("Antal rader i den rensade datan (utan NA):", nrow(data_cleaned), "\n")

# Uppdatera din 'data' dataframe med den rensade versionen
data <- data_cleaned

# Kontrollera att det inte finns n�gra NA-v�rden kvar
cat("Antal NA-v�rden per kolumn i den rensade datan:\n")
print(colSums(is.na(data_cleaned)))


# Ta bort kolumnen 'make'
data <- data[, !(names(data) %in% c("make"))]


# Konvertera registration_date till numeriskt (antal dagar sedan 1970-01-01)
data$registration_date_numeric <- as.numeric(data$registration_date)

# Ber�kna korrelationen mellan det numeriska datumet och modell�ret
correlation <- cor(data$registration_date_numeric, data$model_year, use = "complete.obs")
cat("Korrelation mellan registration_date och model_year:", correlation, "\n")

# Om du best�mmer dig f�r att ta bort registration_date
data <- data[, !(names(data) %in% c("registration_date", "registration_date_numeric"))]

# Kontrollera kolumnerna igen
names(data)


# Konvertera registration_date till numeriskt tempor�rt
registration_date_numeric_temp <- as.numeric(as.Date(data$registration_date))

# Ber�kna korrelationen mellan det numeriska datumet och modell�ret
correlation <- cor(registration_date_numeric_temp, data$model_year, use = "complete.obs")
cat("Korrelation mellan registration_date och model_year:", correlation, "\n")
#Korrelationen blev 0,1985

# Ber�kna korrelationen
year_registration_all <- as.numeric(format(as.Date(data$registration_date), "%Y"))
correlation_by_year_updated_again <- cor(year_registration_all, data$model_year, use = "complete.obs")
cat("Uppdaterad korrelation mellan registrerings�r och modell�r:", correlation_by_year_updated_again, "\n")
#Blev 0,9868 s� det �r v�ldigt stark korrelation -> ta bort datum i trafik


# Ta bort kolumnen 'registration_date'
data$registration_date <- NULL


# Konvertera kolumnen till integer
data$selling_price <- as.integer(data$selling_price)



# Ange s�kv�g och filnamn f�r din CSV-fil i samma mapp som indatafilen
mapp_path <- "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/"
file_name <- "cleaned_volkswagen_data.csv"
complete_path <- paste0(mapp_path, file_name)

# Exportera dataframe:n till CSV
write.csv(data, file = complete_path, row.names = FALSE, fileEncoding = "UTF-8")

cat(paste("Din rensade data har exporterats till:", complete_path, "\n"))


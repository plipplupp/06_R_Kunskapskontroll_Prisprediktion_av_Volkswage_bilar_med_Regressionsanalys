install.packages(c("tidyverse", "corrplot", "patchwork"))
library(tidyverse)
library(ggplot2)
library(corrplot)
library(patchwork) # För att kombinera ggplot-plottar

# Sätter tema för ggplot
theme_set(theme_minimal())

# Läs in CSV-filen
data <- read.csv("C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/cleaned_volkswagen_data.csv", fileEncoding = "UTF-8")

#Ändrar priset på en bil, saknades en nolla.
data <- data %>%
  mutate(selling_price = ifelse(model_year == 2022 & model == "tiguan" & selling_price == 31900,
                                319000,
                                selling_price))

#EDA
head(data)
str(data)
summary(data)
#1182 rader kvar från ursprungliga 1204

# Histogram för selling_price
ggplot(data, aes(x = selling_price)) +
  geom_histogram(binwidth = 50000, fill = "steelblue", color = "black") +
  labs(title = "Histogram över försäljningspris",
       x = "Pris (SEK)",
       y = "Frekvens") +
  theme_minimal()

# Densitetsplott för selling_price
ggplot(data, aes(x = selling_price)) +
  geom_density(fill = "lightcoral", alpha = 0.7) +
  labs(title = "Densitetsplott över försäljningspris",
       x = "Försäljningspris (SEK)",
       y = "Densitet") +
  theme_minimal()

# Boxplott för selling_price
ggplot(data, aes(y = selling_price)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplott över Selling Price",
       y = "Selling Price (SEK)") +
  theme_minimal()

#Scatterplot över pris och årsmodell med olika märken
ggplot(data, aes(x = model_year, y = selling_price, color = model)) +
  geom_point() +
  labs(title = "Pris vs Årsmodell, färgat efter Modell",
       x = "Årsmodell",
       y = "Pris (SEK)",
       color = "Modell") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))

# Spara den senaste ggplot som en PNG-fil
ggsave(filename = "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/densitetsplott_försäljningspris.png",
       plot = last_plot(),
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Ändrar priset på en bil, saknades en nolla.
data <- data %>%
  mutate(selling_price = ifelse(model_year == 2022 & model == "tiguan" & selling_price == 31900,
                                319000,
                                selling_price))

#Korrelationsmatris
# Välj de numeriska kolumner vi är intresserade av
numeric_data <- data %>%
  select(selling_price, mileage, model_year, horsepower)

# Beräkna korrelationsmatrisen
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Visualisera korrelationsmatrisen med corrplot
corrplot(correlation_matrix, method = "circle", type = "lower",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45)

#Spara som bild
filsökväg_png <- "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/korrelationsmatris.png"

# Starta PNG-grafikenheten
png(filename = filsökväg_png, width = 800, height = 800)

# Kör koden för att skapa korrelationsplotten
numeric_data <- data %>%
  select(selling_price, mileage, model_year, horsepower)
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
corrplot(correlation_matrix, method = "circle", type = "lower",
         addCoef.col = "black", number.cex = 1.6,
         tl.col = "black", tl.srt = 45)

# Stäng av grafikenheten för att spara filen
dev.off()


# Val av variabler--------------------------------------------------------------
# Boxplott för selling_price vs. seller
ggplot(data, aes(x = seller, y = selling_price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Pris vs. Säljare",
       x = "Säljare",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Boxplott för selling_price vs. fuel_type
ggplot(data, aes(x = fuel_type, y = selling_price)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Pris vs. Bränsletyp",
       x = "Bränsletyp",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Boxplott för selling_price vs. body_type
ggplot(data, aes(x = body_type, y = selling_price)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Pris vs. Karosstyp",
       x = "Karosstyp",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Boxplott för selling_price vs. transmission
ggplot(data, aes(x = transmission, y = selling_price)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Pris vs. Transmission",
       x = "Transmission",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Boxplott för selling_price vs. drive_wheel_config
ggplot(data, aes(x = drive_wheel_config, y = selling_price)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Pris vs. Drivhjulskonfiguration",
       x = "Drivhjulskonfiguration",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Boxplott för selling_price vs. color
ggplot(data, aes(x = color, y = selling_price)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Pris vs. Färg",
       x = "Färg",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Roterar x-axelns text för läsbarhet

# Boxplott för selling_price vs. model
ggplot(data, aes(x = model, y = selling_price)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Pris vs. Modell",
       x = "Modell",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) # Roterar och minskar textstorlek

# Boxplott för selling_price vs. region
ggplot(data, aes(x = region, y = selling_price)) +
  geom_boxplot(fill = "magenta", color = "black") +
  labs(title = "Pris vs. Region",
       x = "Region",
       y = "Pris (SEK)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Roterar x-axelns text för läsbarhet


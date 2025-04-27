install.packages("pxweb")
library(pxweb)
library(ggplot2)
library(scales)
library(ggthemes)

d <- pxweb_interactive()

# API-URL för fordonsstatistik
api_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA"

# Hämta metadata
metadata <- pxweb_get(api_url)
names(metadata)
metadata$title
metadata$variables

  
# Skapa frågelista
query_list <- list(
  Region = c("00"), # Hela Sverige
  ContentsCode = c("TK1001AB"), # personbilar i trafik.
  Tid = c("*") # Alla tillgängliga år
)

# Hämta data
data <- pxweb_get(url = api_url, query = query_list)
data_df <- as.data.frame(data)

print(data_df)

# Konvertera kolumnen "år" till en numerisk datatyp.
data_df$år <- as.numeric(data_df$år)


# Anpassad formateringsfunktion med mellanslag som avgränsare
space_comma <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

# Beräkna lutningen
model <- lm(Antal ~ år, data = data_df)
slope <- coef(model)[["år"]]

# Hämta värden för första och sista datapunkt
first_value <- data_df$Antal[1]
last_value <- data_df$Antal[nrow(data_df)]

# Skapa ggplot
p <- ggplot(data_df, aes(x = år, y = Antal)) +
  geom_line(color = "#2196F3", linewidth = 1.5) + # Blå linje
  geom_smooth(method = "lm", se = FALSE, color = "#F44336", linetype = "dashed") + # Streckad trendlinje
  labs(title = "Antal personbilar i trafik i Sverige (2002-2024)",
       x = "År",
       y = "Antal personbilar") +
  theme_gray() + # Lägg till grå bakgrund
  scale_y_continuous(labels = space_comma) +
  geom_text(aes(x = min(år), y = first_value), label = paste("2002:", space_comma(first_value)), hjust = -1, vjust = -1, size = 3) +
  geom_text(aes(x = max(år), y = last_value), label = paste("2024:", space_comma(last_value)), hjust = 1, vjust = 3, size = 3) +
  geom_text(aes(x = mean(år), y = max(Antal)), label = paste("Antalet personbilar ökar i snitt varje år med: ", round(slope, 0), " bilar/år"), hjust = 0.5, vjust = -1, size = 4, color = "#F44336") # Lutning

# Exportera grafen som en JPG-bild
ggsave(filename = "antal_bilar_2002_2024.jpg", plot = p, path = "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll", width = 12, height = 6)

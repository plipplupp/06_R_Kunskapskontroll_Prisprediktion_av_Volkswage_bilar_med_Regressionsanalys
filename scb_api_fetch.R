install.packages("pxweb")
library(pxweb)
library(ggplot2)
library(scales)
library(ggthemes)

d <- pxweb_interactive()

# API-URL f�r fordonsstatistik
api_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA"

# H�mta metadata
metadata <- pxweb_get(api_url)
names(metadata)
metadata$title
metadata$variables

  
# Skapa fr�gelista
query_list <- list(
  Region = c("00"), # Hela Sverige
  ContentsCode = c("TK1001AB"), # personbilar i trafik.
  Tid = c("*") # Alla tillg�ngliga �r
)

# H�mta data
data <- pxweb_get(url = api_url, query = query_list)
data_df <- as.data.frame(data)

print(data_df)

# Konvertera kolumnen "�r" till en numerisk datatyp.
data_df$�r <- as.numeric(data_df$�r)


# Anpassad formateringsfunktion med mellanslag som avgr�nsare
space_comma <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

# Ber�kna lutningen
model <- lm(Antal ~ �r, data = data_df)
slope <- coef(model)[["�r"]]

# H�mta v�rden f�r f�rsta och sista datapunkt
first_value <- data_df$Antal[1]
last_value <- data_df$Antal[nrow(data_df)]

# Skapa ggplot
p <- ggplot(data_df, aes(x = �r, y = Antal)) +
  geom_line(color = "#2196F3", linewidth = 1.5) + # Bl� linje
  geom_smooth(method = "lm", se = FALSE, color = "#F44336", linetype = "dashed") + # Streckad trendlinje
  labs(title = "Antal personbilar i trafik i Sverige (2002-2024)",
       x = "�r",
       y = "Antal personbilar") +
  theme_gray() + # L�gg till gr� bakgrund
  scale_y_continuous(labels = space_comma) +
  geom_text(aes(x = min(�r), y = first_value), label = paste("2002:", space_comma(first_value)), hjust = -1, vjust = -1, size = 3) +
  geom_text(aes(x = max(�r), y = last_value), label = paste("2024:", space_comma(last_value)), hjust = 1, vjust = 3, size = 3) +
  geom_text(aes(x = mean(�r), y = max(Antal)), label = paste("Antalet personbilar �kar i snitt varje �r med: ", round(slope, 0), " bilar/�r"), hjust = 0.5, vjust = -1, size = 4, color = "#F44336") # Lutning

# Exportera grafen som en JPG-bild
ggsave(filename = "antal_bilar_2002_2024.jpg", plot = p, path = "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll", width = 12, height = 6)

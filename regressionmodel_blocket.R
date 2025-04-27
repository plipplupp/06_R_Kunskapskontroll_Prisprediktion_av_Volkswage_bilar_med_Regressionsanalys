install.packages(c("dplyr", "car", "lmtest", "sandwich")) # Installera vid behov
library(dplyr)
library(car)
library(lmtest)
library(sandwich) # För vcovHC

# 1. Läs in och förbereder data
# --------------------------------------------------------------------------
cat("Läser in och förbereder data...\n")
file_path <- "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/cleaned_volkswagen_data.csv"
data <- read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Justerar två värden på tre bilar
data$selling_price[which(data$selling_price == 31900 & data$model_year == 2022 & data$model == "tiguan")] <- 319000
data$model[which(data$model_year == 2019 & data$model == "golf" & data$selling_price == 159900 & data$mileage == 7670 & data$horsepower == 136 & data$seller == "företag" & data$fuel_type == "el")] <- "e-golf"
data$fuel_type[which(data$selling_price == 279000 & data$mileage == 14500 & data$model_year == 2021 & data$horsepower == 150)] <- "el"



# Lägger alla bilmodeller färre än 20 i en övrigt kategori
antal_per_modell <- data |> count(model, sort = TRUE, name = "antal")
modeller_att_behalla <- antal_per_modell |> filter(antal >= 20) |> pull(model)
data <- data |> mutate(model_reduced = ifelse(model %in% modeller_att_behalla, model, "övrigt"))
data$model <- NULL

# Plockar bort features
#---------------------------------------------------------------------------

# Räknar lite bilar
antal_aldre_an_2010_innan <- data |>
  (\(df) sum(df$model_year < 2010, na.rm = TRUE))()

cat("Antal bilar äldre än 2010 (innan borttagning):", antal_aldre_an_2010_innan, "\n")

antal_bilar_under_pris <- data |>
  (\(df) sum(df$selling_price < 20000, na.rm = TRUE))()

cat("Antal bilar med ett pris under 20 000 kr:", antal_bilar_under_pris, "\n")

# Temporär reducerad dataframe 'data_reduced'
data_reduced <- data |>
  (\(df) df |>
     # 1. Ta bort kolumnen 'transmission'
     select(-transmission) |>
     
     # 2. Ta bort kolumnen 'body_type'
     select(-body_type) |>
     
     # 3. Ta bort rader där 'model_reduced' är "övrigt"
     filter(model_reduced != "övrigt") |>
     
     # 4. Ta bort bilar äldre än 2010
     filter(model_year >= 2010)

  )()

# Kontrollera dimensioner efter rensning
cat("\nDimensioner av den ursprungliga datan:", dim(data), "\n")
cat("Dimensioner av den reducerade datan:", dim(data_reduced), "\n")

# Tilldela den reducerade datan till variabeln 'data'
data <- data_reduced

# Skapar faktorvariabler som sedan kan skapa dummyvariabler. Olika vategoriacal_vars beroende på om kolumner tagist bort
categorical_vars <- c("seller", "fuel_type", "model_reduced")
#categorical_vars <- c("seller", "fuel_type", "body_type", "model_reduced", "transmission")
categorical_vars <- intersect(categorical_vars, names(data))
data <- data |> mutate(across(all_of(categorical_vars), as.factor))

# Prefixa nivåerna med variabelnamn och understreck
for (col in categorical_vars) {
  if (is.factor(data[[col]])) {
    levels(data[[col]]) <- paste0(col, "_", levels(data[[col]]))
  }
}

cols_for_model <- c("selling_price", "mileage", "model_year", "horsepower", categorical_vars)
# Kontrollera att alla kolumner finns innan na.omit
cols_for_model <- intersect(cols_for_model, names(data))
data <- na.omit(data[, cols_for_model])
cat("Antal rader efter NA-borttagning:", nrow(data), "\n")



# 2. Dela upp data (60% träning, 20% validering, 20% test)
# --------------------------------------------------------------------------
cat("Delar upp data...\n")
spec = c(train = .6, validate = .2, test = .2)
set.seed(123)
n_rows <- nrow(data)
groups = sample(cut(seq(n_rows), n_rows * cumsum(c(0, spec)), labels = names(spec)))
res = split(data, groups)
train_data <- res$train
val_data <- res$validate
test_data <- res$test
cat("Träningsdata:", nrow(train_data), "rader\n")
cat("Valideringsdata:", nrow(val_data), "rader\n")
cat("Testdata:", nrow(test_data), "rader\n")

# 3. Träna modeller
# --------------------------------------------------------------------------
cat("\n--- Tränar Modell 1 (Linjär) ---\n")
lm_1 <- lm(selling_price ~ ., data = train_data)
print(summary(lm_1))

cat("\n--- Tränar Modell 2 (Log-Linjär) ---\n")
lm_2 <- lm(log(selling_price) ~ ., data = train_data)
print(summary(lm_2))

# Modell 3: Log-modell + interaktioner, interaktion mellan årsmodell och miltal, samt hk och bränsletyp
cat("\n--- Tränar Modell 3 (Log-Linjär med Interaktioner) ---\n")
lm_3_interaction <- lm(log(selling_price) ~ . + model_year:mileage + horsepower:fuel_type, data = train_data)
print(summary(lm_3_interaction))



# 4. Diagnostik för alla modeller
# --------------------------------------------------------------------------
cat("\n--- Diagnostikplottar ---\n")

# Funktion för att visa standard lm-plottar
plot_lm_diagnostics <- function(model, model_name) {
  if(is.null(model)) { # Hoppa över om modellen inte skapades
    cat("Hoppar över diagnostik för:", model_name, "(modellen är NULL)\n")
    return()
  }
  cat("\nDiagnostik för:", model_name, "\n")
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) # Justerade marginaler & titelutrymme
  tryCatch({
    plot(model, main = model_name) # Ta bort individuell titel för renare look
    # Lägg till en övergripande titel med title() istället för mtext() om så önskas
    title(main = paste("Diagnostik:", model_name), outer = FALSE, line = 1.5) # Titel närmare plottarna
  }, error = function(e) {
    message("Kunde inte skapa diagnostikplott för ", model_name, ": ", conditionMessage(e))
  })
  par(mfrow = c(1, 1)) # Återställ plot-layout
}

# Visa plottar
plot_lm_diagnostics(lm_1, "Modell 1 (Linjär)")
plot_lm_diagnostics(lm_2, "Modell 2 (Log-Linjär)")
plot_lm_diagnostics(lm_3_interaction, "Modell 3 (Log + Interaktion)")



# ***********************************************************************************
# points_of_interest_indices <- c(582, 877, 1156, 890, 834, 811, 377, 134, 477, 915, 539, 641, 581)
#points_of_interest_indices <- c(915, 539, 641, 581, 509, 334, 843, 210)
points_of_interest_indices <- c(333, 322, 670)

# 3. Plocka ut raderna direkt baserat på index
points_of_interest <- data[points_of_interest_indices, ]

# 4. Visa informationen
print("Information om de intressanta bilarna:")
print(points_of_interest)
# ***********************************************************************************


# Ytterligare diagnostik (VIF och bptest)
cat("\n--- Ytterligare Diagnostik (VIF & Heteroskedasticitet) ---\n")

run_diagnostics <- function(model, model_name) {
  if(is.null(model)) {
    cat("Hoppar över VIF/bptest för:", model_name, "(modellen är NULL)\n")
    return()
  }
  cat("\nDiagnostik för:", model_name, "\n")
  # VIF
  cat(" VIF:\n")
  tryCatch({
    vif_result <- vif(model)
    # Hantera om VIF returnerar en matris (för faktorer med >2 nivåer)
    if (is.matrix(vif_result)) {
      print(vif_result)
    } else {
      print(head(sort(vif_result, decreasing = TRUE)))
    }
  }, error = function(e) message("  VIF-fel: ", conditionMessage(e)))
  # Breusch-Pagan test
  cat(" Breusch-Pagan Test (Homoskedasticitet):\n")
  tryCatch({ print(bptest(model)) }, error = function(e) message("  bptest-fel: ", conditionMessage(e)))
}

run_diagnostics(lm_1, "Modell 1 (Linjär)")
run_diagnostics(lm_2, "Modell 2 (Log-Linjär)")
run_diagnostics(lm_3_interaction, "Modell 3 (Log + Interaktion)")




# 5. Jämför modeller på Valideringsdata (RMSE)
# --------------------------------------------------------------------------
cat("\n--- Jämför modeller på Valideringsdata (RMSE) ---\n")

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE)) # la till na.rm
}

# Skapa lista för resultat
rmse_results <- list()

# Modell 1
pred_1_val <- predict(lm_1, newdata = val_data, na.action = na.pass)
pred_1_val[pred_1_val < 0] <- 1
rmse_results[["Linjär"]] <- calculate_rmse(val_data$selling_price, pred_1_val)

# Modell 2
pred_2_log_val <- predict(lm_2, newdata = val_data, na.action = na.pass)
pred_2_price_val <- exp(pred_2_log_val)
rmse_results[["Log-Linjär"]] <- calculate_rmse(val_data$selling_price, pred_2_price_val)

# Modell 3
pred_3_log_val <- predict(lm_3_interaction, newdata = val_data, na.action = na.pass)
pred_3_price_val <- exp(pred_3_log_val)
rmse_results[["Log + Interaktion"]] <- calculate_rmse(val_data$selling_price, pred_3_price_val)


# Skapa och skriv ut resultat-df
results_val <- data.frame(
  Model = names(rmse_results),
  Validation_RMSE = unlist(rmse_results)
)
# Sortera efter RMSE
results_val <- results_val[order(results_val$Validation_RMSE), ]
print(results_val)

# 6. Välj bästa modell och utvärdera på Testdata
# --------------------------------------------------------------------------
cat("\n--- Väljer bästa modell och utvärderar på Testdata ---\n")

best_model_name <- results_val$Model[1] # Ta den med lägst RMSE
best_model_object <- switch(best_model_name,
                            "Linjär" = lm_1,
                            "Log-Linjär" = lm_2,
                            "Log + Interaktion" = lm_3_interaction)

cat("Bästa modell baserat på Validering RMSE:", best_model_name, "\n")

# Beräkna Test RMSE för den bästa modellen
if (best_model_name == "Linjär") {
  pred_best_test <- predict(best_model_object, newdata = test_data, na.action = na.pass)
  pred_best_test[pred_best_test < 0] <- 1
  rmse_test_final <- calculate_rmse(test_data$selling_price, pred_best_test)
} else { # Log-baserad modell
  pred_best_log_test <- predict(best_model_object, newdata = test_data, na.action = na.pass)
  pred_best_price_test <- exp(pred_best_log_test)
  rmse_test_final <- calculate_rmse(test_data$selling_price, pred_best_price_test)
}

cat("Slutlig Test RMSE (för vald modell):", rmse_test_final, "\n")

# 7. Prediktion för två nya bilar
# --------------------------------------------------------------------------
cat("\n--- Prediktion för två nya bilar ---\n")

print(categorical_vars)
print(names(train_data))

# Hämta faktornivåer från träningsdatan
factor_levels <- lapply(train_data[, categorical_vars, drop = FALSE], levels)

# # Två nya bilar (med transmission och body_type)
# new_cars <- data.frame(
#   mileage = c(5000, 12000),
#   model_year = c(2021, 2018),
#   horsepower = c(150, 110),
#   seller = factor(c("företag", "privat"), levels = factor_levels$seller),
#   fuel_type = factor(c("bensin", "diesel"), levels = factor_levels$fuel_type),
#   transmission = factor(c("manuell", "automat"), levels = factor_levels$transmission),
#   body_type = factor(c("halvkombi", "suv"), levels = factor_levels$body_type),
#   model_reduced = factor(c("golf", "tiguan"), levels = factor_levels$model_reduced)
# )

# Två nya bilar
new_cars <- data.frame(
  new_cars <- data.frame(
    mileage = c(5000, 12000),
    model_year = c(2021, 2018),
    horsepower = c(150, 110),
    seller = factor(c("företag", "privat"), levels = factor_levels$seller),
    fuel_type = factor(c("bensin", "diesel"), levels = factor_levels$fuel_type),
    model_reduced = factor(c("golf", "tiguan"), levels = factor_levels$model_reduced)
  )
)



# Gör prediktion med prediktionsintervall
predictions_new <- predict(best_model_object, newdata = new_cars, interval = "prediction", level = 0.95)


# Återtransformera om bästa modellen är log-baserad
if (!is.null(predictions_new)) {
  if (best_model_name != "Linjär") {
    predictions_new <- exp(predictions_new)
    cat("Prediktioner (återtransformerade från log-skala):\n")
  } else {
    # Sätt ev negativa gränser till 0 eller 1
    predictions_new[predictions_new[,"lwr"] < 0, "lwr"] <- 1
    predictions_new[predictions_new[,"fit"] < 0, "fit"] <- 1
    cat("Prediktioner (linjär skala):\n")
  }
  print(predictions_new)
}

# 8. Analys av bästa modellen
# --------------------------------------------------------------------------
cat("\n--- Analys av vald modell:", best_model_name, "---\n")
if (!is.null(best_model_object)) {
  print(summary(best_model_object))
  
  cat("\nKonfidensintervall för koefficienter:\n")
  tryCatch({ print(confint(best_model_object)) }, error = function(e) message("Fel vid confint: ", conditionMessage(e)))
  
  cat("\nTest av koefficienter med robusta standardfel (HC1):\n")
  tryCatch({
    # Använd sandwich::vcovHC() explicit
    print(coeftest(best_model_object, vcov = sandwich::vcovHC(best_model_object, type = "HC1")))
  }, error = function(e) message("Fel vid coeftest/vcovHC: ", conditionMessage(e)))
} else {
  cat("Ingen bästa modell kunde väljas eller tränas korrekt.\n")
}

install.packages(c("dplyr", "car", "lmtest", "sandwich")) # Installera vid behov
library(dplyr)
library(car)
library(lmtest)
library(sandwich) # F�r vcovHC

# 1. L�s in och f�rbereder data
# --------------------------------------------------------------------------
cat("L�ser in och f�rbereder data...\n")
file_path <- "C:/Users/Dator/Documents/Data Science/06_R/Mina dokument/Kunskapskontroll/cleaned_volkswagen_data.csv"
data <- read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Justerar tv� v�rden p� tre bilar
data$selling_price[which(data$selling_price == 31900 & data$model_year == 2022 & data$model == "tiguan")] <- 319000
data$model[which(data$model_year == 2019 & data$model == "golf" & data$selling_price == 159900 & data$mileage == 7670 & data$horsepower == 136 & data$seller == "f�retag" & data$fuel_type == "el")] <- "e-golf"
data$fuel_type[which(data$selling_price == 279000 & data$mileage == 14500 & data$model_year == 2021 & data$horsepower == 150)] <- "el"



# L�gger alla bilmodeller f�rre �n 20 i en �vrigt kategori
antal_per_modell <- data |> count(model, sort = TRUE, name = "antal")
modeller_att_behalla <- antal_per_modell |> filter(antal >= 20) |> pull(model)
data <- data |> mutate(model_reduced = ifelse(model %in% modeller_att_behalla, model, "�vrigt"))
data$model <- NULL

# Plockar bort features
#---------------------------------------------------------------------------

# R�knar lite bilar
antal_aldre_an_2010_innan <- data |>
  (\(df) sum(df$model_year < 2010, na.rm = TRUE))()

cat("Antal bilar �ldre �n 2010 (innan borttagning):", antal_aldre_an_2010_innan, "\n")

antal_bilar_under_pris <- data |>
  (\(df) sum(df$selling_price < 20000, na.rm = TRUE))()

cat("Antal bilar med ett pris under 20 000 kr:", antal_bilar_under_pris, "\n")

# Tempor�r reducerad dataframe 'data_reduced'
data_reduced <- data |>
  (\(df) df |>
     # 1. Ta bort kolumnen 'transmission'
     select(-transmission) |>
     
     # 2. Ta bort kolumnen 'body_type'
     select(-body_type) |>
     
     # 3. Ta bort rader d�r 'model_reduced' �r "�vrigt"
     filter(model_reduced != "�vrigt") |>
     
     # 4. Ta bort bilar �ldre �n 2010
     filter(model_year >= 2010)

  )()

# Kontrollera dimensioner efter rensning
cat("\nDimensioner av den ursprungliga datan:", dim(data), "\n")
cat("Dimensioner av den reducerade datan:", dim(data_reduced), "\n")

# Tilldela den reducerade datan till variabeln 'data'
data <- data_reduced

# Skapar faktorvariabler som sedan kan skapa dummyvariabler. Olika vategoriacal_vars beroende p� om kolumner tagist bort
categorical_vars <- c("seller", "fuel_type", "model_reduced")
#categorical_vars <- c("seller", "fuel_type", "body_type", "model_reduced", "transmission")
categorical_vars <- intersect(categorical_vars, names(data))
data <- data |> mutate(across(all_of(categorical_vars), as.factor))

# Prefixa niv�erna med variabelnamn och understreck
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



# 2. Dela upp data (60% tr�ning, 20% validering, 20% test)
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
cat("Tr�ningsdata:", nrow(train_data), "rader\n")
cat("Valideringsdata:", nrow(val_data), "rader\n")
cat("Testdata:", nrow(test_data), "rader\n")

# 3. Tr�na modeller
# --------------------------------------------------------------------------
cat("\n--- Tr�nar Modell 1 (Linj�r) ---\n")
lm_1 <- lm(selling_price ~ ., data = train_data)
print(summary(lm_1))

cat("\n--- Tr�nar Modell 2 (Log-Linj�r) ---\n")
lm_2 <- lm(log(selling_price) ~ ., data = train_data)
print(summary(lm_2))

# Modell 3: Log-modell + interaktioner, interaktion mellan �rsmodell och miltal, samt hk och br�nsletyp
cat("\n--- Tr�nar Modell 3 (Log-Linj�r med Interaktioner) ---\n")
lm_3_interaction <- lm(log(selling_price) ~ . + model_year:mileage + horsepower:fuel_type, data = train_data)
print(summary(lm_3_interaction))



# 4. Diagnostik f�r alla modeller
# --------------------------------------------------------------------------
cat("\n--- Diagnostikplottar ---\n")

# Funktion f�r att visa standard lm-plottar
plot_lm_diagnostics <- function(model, model_name) {
  if(is.null(model)) { # Hoppa �ver om modellen inte skapades
    cat("Hoppar �ver diagnostik f�r:", model_name, "(modellen �r NULL)\n")
    return()
  }
  cat("\nDiagnostik f�r:", model_name, "\n")
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) # Justerade marginaler & titelutrymme
  tryCatch({
    plot(model, main = model_name) # Ta bort individuell titel f�r renare look
    # L�gg till en �vergripande titel med title() ist�llet f�r mtext() om s� �nskas
    title(main = paste("Diagnostik:", model_name), outer = FALSE, line = 1.5) # Titel n�rmare plottarna
  }, error = function(e) {
    message("Kunde inte skapa diagnostikplott f�r ", model_name, ": ", conditionMessage(e))
  })
  par(mfrow = c(1, 1)) # �terst�ll plot-layout
}

# Visa plottar
plot_lm_diagnostics(lm_1, "Modell 1 (Linj�r)")
plot_lm_diagnostics(lm_2, "Modell 2 (Log-Linj�r)")
plot_lm_diagnostics(lm_3_interaction, "Modell 3 (Log + Interaktion)")



# ***********************************************************************************
# points_of_interest_indices <- c(582, 877, 1156, 890, 834, 811, 377, 134, 477, 915, 539, 641, 581)
#points_of_interest_indices <- c(915, 539, 641, 581, 509, 334, 843, 210)
points_of_interest_indices <- c(333, 322, 670)

# 3. Plocka ut raderna direkt baserat p� index
points_of_interest <- data[points_of_interest_indices, ]

# 4. Visa informationen
print("Information om de intressanta bilarna:")
print(points_of_interest)
# ***********************************************************************************


# Ytterligare diagnostik (VIF och bptest)
cat("\n--- Ytterligare Diagnostik (VIF & Heteroskedasticitet) ---\n")

run_diagnostics <- function(model, model_name) {
  if(is.null(model)) {
    cat("Hoppar �ver VIF/bptest f�r:", model_name, "(modellen �r NULL)\n")
    return()
  }
  cat("\nDiagnostik f�r:", model_name, "\n")
  # VIF
  cat(" VIF:\n")
  tryCatch({
    vif_result <- vif(model)
    # Hantera om VIF returnerar en matris (f�r faktorer med >2 niv�er)
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

run_diagnostics(lm_1, "Modell 1 (Linj�r)")
run_diagnostics(lm_2, "Modell 2 (Log-Linj�r)")
run_diagnostics(lm_3_interaction, "Modell 3 (Log + Interaktion)")




# 5. J�mf�r modeller p� Valideringsdata (RMSE)
# --------------------------------------------------------------------------
cat("\n--- J�mf�r modeller p� Valideringsdata (RMSE) ---\n")

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE)) # la till na.rm
}

# Skapa lista f�r resultat
rmse_results <- list()

# Modell 1
pred_1_val <- predict(lm_1, newdata = val_data, na.action = na.pass)
pred_1_val[pred_1_val < 0] <- 1
rmse_results[["Linj�r"]] <- calculate_rmse(val_data$selling_price, pred_1_val)

# Modell 2
pred_2_log_val <- predict(lm_2, newdata = val_data, na.action = na.pass)
pred_2_price_val <- exp(pred_2_log_val)
rmse_results[["Log-Linj�r"]] <- calculate_rmse(val_data$selling_price, pred_2_price_val)

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

# 6. V�lj b�sta modell och utv�rdera p� Testdata
# --------------------------------------------------------------------------
cat("\n--- V�ljer b�sta modell och utv�rderar p� Testdata ---\n")

best_model_name <- results_val$Model[1] # Ta den med l�gst RMSE
best_model_object <- switch(best_model_name,
                            "Linj�r" = lm_1,
                            "Log-Linj�r" = lm_2,
                            "Log + Interaktion" = lm_3_interaction)

cat("B�sta modell baserat p� Validering RMSE:", best_model_name, "\n")

# Ber�kna Test RMSE f�r den b�sta modellen
if (best_model_name == "Linj�r") {
  pred_best_test <- predict(best_model_object, newdata = test_data, na.action = na.pass)
  pred_best_test[pred_best_test < 0] <- 1
  rmse_test_final <- calculate_rmse(test_data$selling_price, pred_best_test)
} else { # Log-baserad modell
  pred_best_log_test <- predict(best_model_object, newdata = test_data, na.action = na.pass)
  pred_best_price_test <- exp(pred_best_log_test)
  rmse_test_final <- calculate_rmse(test_data$selling_price, pred_best_price_test)
}

cat("Slutlig Test RMSE (f�r vald modell):", rmse_test_final, "\n")

# 7. Prediktion f�r tv� nya bilar
# --------------------------------------------------------------------------
cat("\n--- Prediktion f�r tv� nya bilar ---\n")

print(categorical_vars)
print(names(train_data))

# H�mta faktorniv�er fr�n tr�ningsdatan
factor_levels <- lapply(train_data[, categorical_vars, drop = FALSE], levels)

# # Tv� nya bilar (med transmission och body_type)
# new_cars <- data.frame(
#   mileage = c(5000, 12000),
#   model_year = c(2021, 2018),
#   horsepower = c(150, 110),
#   seller = factor(c("f�retag", "privat"), levels = factor_levels$seller),
#   fuel_type = factor(c("bensin", "diesel"), levels = factor_levels$fuel_type),
#   transmission = factor(c("manuell", "automat"), levels = factor_levels$transmission),
#   body_type = factor(c("halvkombi", "suv"), levels = factor_levels$body_type),
#   model_reduced = factor(c("golf", "tiguan"), levels = factor_levels$model_reduced)
# )

# Tv� nya bilar
new_cars <- data.frame(
  new_cars <- data.frame(
    mileage = c(5000, 12000),
    model_year = c(2021, 2018),
    horsepower = c(150, 110),
    seller = factor(c("f�retag", "privat"), levels = factor_levels$seller),
    fuel_type = factor(c("bensin", "diesel"), levels = factor_levels$fuel_type),
    model_reduced = factor(c("golf", "tiguan"), levels = factor_levels$model_reduced)
  )
)



# G�r prediktion med prediktionsintervall
predictions_new <- predict(best_model_object, newdata = new_cars, interval = "prediction", level = 0.95)


# �tertransformera om b�sta modellen �r log-baserad
if (!is.null(predictions_new)) {
  if (best_model_name != "Linj�r") {
    predictions_new <- exp(predictions_new)
    cat("Prediktioner (�tertransformerade fr�n log-skala):\n")
  } else {
    # S�tt ev negativa gr�nser till 0 eller 1
    predictions_new[predictions_new[,"lwr"] < 0, "lwr"] <- 1
    predictions_new[predictions_new[,"fit"] < 0, "fit"] <- 1
    cat("Prediktioner (linj�r skala):\n")
  }
  print(predictions_new)
}

# 8. Analys av b�sta modellen
# --------------------------------------------------------------------------
cat("\n--- Analys av vald modell:", best_model_name, "---\n")
if (!is.null(best_model_object)) {
  print(summary(best_model_object))
  
  cat("\nKonfidensintervall f�r koefficienter:\n")
  tryCatch({ print(confint(best_model_object)) }, error = function(e) message("Fel vid confint: ", conditionMessage(e)))
  
  cat("\nTest av koefficienter med robusta standardfel (HC1):\n")
  tryCatch({
    # Anv�nd sandwich::vcovHC() explicit
    print(coeftest(best_model_object, vcov = sandwich::vcovHC(best_model_object, type = "HC1")))
  }, error = function(e) message("Fel vid coeftest/vcovHC: ", conditionMessage(e)))
} else {
  cat("Ingen b�sta modell kunde v�ljas eller tr�nas korrekt.\n")
}

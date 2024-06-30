#<#############################################################################>
# Times series und Forecasting----
#<#############################################################################>

install.packages("modeltime", dependencies = TRUE)


# Laden der Libraries ----
#<#############################################################################>

library(tidyverse)
library(tidymodels)
library(timetk)
library(lubridate)
library(modeltime)
library(readr)
library(dplyr)



# Einlesen der Daten----
#<#############################################################################>

# Zur Einsicht der Daten view(bike_Sharing_daily) ausführen. Der Datensatz ist
# bereits im Modeltime package enthalten.

view(bike_sharing_daily)
str(bike_sharing_daily)

data <- bike_sharing_daily %>%
  select(dteday, cnt)

data %>% plot_time_series(dteday, cnt)


# Time series split----
#<#############################################################################>

# Train & Test Splits

splits <- time_series_split(
  data,
  assess     = "3 months",
  cumulative = TRUE
)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dteday, cnt)

#> 1. Aufgabe: Cross Validation----
#<#############################################################################>
# Erstelle eine Cross Validation, die den Datensatz aufteilt in 
# 80% Trainingsdaten und 20% Testdaten und visualisiere die Aufteilung.




# Modellierung mit Modeltime----
#<#############################################################################>


# > 2. Aufgabe: AUTO ARIMA----
#<#############################################################################>
# Erstelle ein ARIMA Modell mit dem auto_arima Algorithmus und trainiere es mit 
# den Trainingsdaten. Erkläre den Verlauf von ARIMA auf den Testdaten!




#>======================================================<#
# Führe diesen Teil aus, um dein Modell zu visualisieren #
#>======================================================<#
arima_forecast <- model_arima %>%
  modeltime_table() %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data
  )
arima_forecast %>%
  plot_modeltime_forecast(
    .interactive = TRUE,
    .title = "ARIMA Modell - Testdatenvorhersage"
  )
#>======================================================<#
# Führe diesen Teil aus, um dein Modell zu visualisieren #
#>======================================================<#


# > 3. Aufgabe: Prophet ----
#<#############################################################################>
# Erstelle ein Prophet Modell mit der seasonality_yearly Option und trainiere
# es mit den Trainingsdaten. Erkläre den Verlauf von Prophet auf den Testdaten!




#>=======================================================<#
# Führe diesen Teil aus, um dein Modell zu visualisieren  #
#>=======================================================<#
prophet_forecast <- model_prophet %>%
  modeltime_table() %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data
  )
prophet_forecast %>%
  plot_modeltime_forecast(
    .interactive = TRUE,
    .title = "Prophet Modell - Testdatenvorhersage"
  )
#>======================================================<#
# Führe diesen Teil aus, um dein Modell zu visualisieren #
#>======================================================<#


# Machine Learning - GLM ----
#<#############################################################################>
model_glmnet <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(
    cnt ~ wday(dteday, label = TRUE)
    + month(dteday, label = TRUE)
    + as.numeric(dteday),
    training(splits)
  )

model_glmnet

# Modeltime Compare ----
#<#############################################################################>

# Modeltime Table ----
model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_glmnet
)

# Calibration ----
calib_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

# Accuracy ----
calib_tbl %>% modeltime_accuracy()

# > 4. Aufgabe: Test Set Visualisierung ----
#<#############################################################################>
# Visualisiere die Testdaten mit den Modellen aus der calib_tbl. Welches Modell
# liefert die besten Vorhersagen?




# > 5. Aufgabe: Forecast Future ----
#<#############################################################################>
# Erstelle eine Vorhersage für die nächsten 3 Monate mit den Modellen aus der
# calib_tbl. Visualisiere die Vorhersage mit plot_modeltime_forecast().
# Erläutere die Vorhersage.




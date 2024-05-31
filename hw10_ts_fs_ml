# --------------------------------------------
# Script Name: ts_fs_ml
# Purpose1: Creating a timeseries object and visualization.
# Purpose2: Extracting the features of this timeseries and building a forecast model with tidymodels package.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-05-25
#
# --------------------------------------------

cat("\014") # Clears the console
rm(list = ls()) # Remove all variables


# Step1 Uploading and clean data

# Load packages and the data of Doubs

library(tidyverse)
library(lubridate) # manipulating dates
library(devtools)
library(tidymodels)
library(forecast) # work with ggplot2 for autoplot()
library(ggplot2)
library(timetk)

fish <- read.table('D:/中科大研一第二学期/数据驱动生态学/Prunier et al._RawBiomassData.txt',h=TRUE)

# Data pre-processing 

head(fish)
anyNA(fish)

any_missing <- any(is.na(data))
print(paste("Does 'data' have missing data?", any_missing))

# Remove the missing and duplicated data

fish_clean <- fish |>
  drop_na() |>
  distinct() 
view(fish_clean)

# Identify and count stations and species

unique(fish_clean$STATION) 
table(fish_clean$STATION)
unique(fish_clean$SP) 
table(fish_clean$SP)


# Step2 Create timeseries objects and visualization

# Select the species of VAI which station is VERCah
# Because there are 2 samples for the same dates, they are averaged after examination
fish_selected <- aggregate(cbind(BIOMASS, DENSITY) ~ DATE,
                           subset(fish_clean, STATION == "VERCah" & SP == "VAI"),
                           FUN = mean)
view(fish_selected)

# Method1 - Create a time series using the 'ts' function
fish_ts1 = ts(data = fish_selected[,-1], 
              start = c(1994), 
              frequency = 1)  # Annual frequency, assuming one observation per year
fish_ts1

# Plot the time series data focused on the density of VAI in Doubs river

library(forecast) # Load the 'forecast' package for using 'autoplot' function, which works well with ggplot2 for visualization
library(ggplot2)
autoplot(fish_ts1) +
  ggtitle("timeseries about changes of VAI in Doubs river") +
  ylab("Changes") + xlab("Year")

# Method2 - Create a time series using the 'timetk' function

library(timetk)
library(tidyverse)
library(tsibble)

VERCah_VAI <- fish_clean |>
  subset(STATION=="VERCah" & SP == "VAI")
view(VERCah_VAI)

fish_ts2 <- VERCah_VAI |>  
  tk_tbl()|> #Convert the data into a 'timetk' table,
  rename(date = DATE) |>
  select(-BIOMASS)|>
  pivot_longer( # convert to to long format
    cols ="DENSITY") 
view(fish_ts2)

fish_timetk <- plot_time_series(fish_ts2,date, value, # plot with timetk
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "timeseries for VAI of VERCah Station ")
fish_timetk   

#Step3 missing imputation of timeseries objects
# Note: In order to generate lag and moving window features, the timeseries must be continuous without missing data,
# so the following step aims to execute linear imputation by "timetk" to fill in the gap

# 3.1 Determining missing values 

library(DataExplorer)
library(ggthemes)

fish_ts_miss <- 
  fish_ts2 |>
  summarise_by_time(
    date, 
    .by = "year",
    value = mean(value) ) |>
  pad_by_time(date, .by = "year")

view(fish_ts_miss)

fish_ts_miss |>
  plot_missing(
    ggtheme = theme_calc(), 
    title = "Visulaization of Missing Values"
  )

# 3.2 Imputation of missing data

fish_ts_imp <- fish_ts_miss |>
  mutate_at(vars(value), .funs = ts_impute_vec, period = 1) 
view(fish_ts_imp)

fish_ts_imp |>
  plot_time_series(date, value, 
                   .facet_ncol = 2, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI of Le Doubs river") 

tail(fish_ts_imp)

# 3.3 Find the outlier in timeseries
# Method1
  plot_anomaly_diagnostics(
    .data = fish_ts_imp,
    .date = date,
    .value = value,
    .interactive=FALSE,
    .message = TRUE,
    .title = "Anomaly Detection Report",
    .anom_color ="#00BFFF", 
    .ribbon_alpha = 0.15, #Fill opacity for the acceptable range
    .max_anomalies = 0.07, #The maximum percent of anomalies permitted to be identified
    .alpha = 0.08 #Controls the width of the "normal" range. Lower values are more conservative while higher values are less prone to incorrectly classifying "normal" observations.
  )
#frequency = 5 observations per 5 years，trend = 10 observations per 10 years

# Method2
# Caculate the value of outlier
fish_ts_imp2 <- fish_ts_imp$anomaly <- ifelse(fish_ts_imp$value > fish_ts_imp$value * 0.07, TRUE, FALSE)

# Create object of ggplot
p <- ggplot(fish_ts_imp, aes(x = date, y = value, color = factor(is_anomaly))) +
  geom_line(size = 1) +
  geom_point(data = filter(fish_ts_imp, is_anomaly), color = "#FF69B4", size = 3, alpha = 0.8) +
  facet_wrap(~ name, ncol = 2) +
  scale_color_manual(values = c("#000000", "#FF69B4"), labels = c("Not Anomaly", "Anomaly")) +
  labs(title = "Anomaly Diagnostics",
       x = "Date",
       y = "Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

print(p)


# Step4 Serial autocorrelation 
# Method1 

fish_acf_lag6 <- plot_acf_diagnostics(
    fish_ts_imp,
    date, value,               
    .lags = "6 years",    
    .interactive = FALSE
  )
fish_acf_lag6

fish_ts <- ts(fish_ts_imp$value, start = min(fish_ts_imp$date), frequency = 12)

# Method2
# Caculate ACF and PACF
acf_result <- acf(fish_ts_imp, type="correlation",
                  main = "ACF Plot", 
                  lag.max = 6,
                  plot = FALSE)
pacf_result <- pacf(fish_ts_imp, main = "PACF Plot", plot = FALSE)

# Plot ACF and PACF
autoplot(acf_result) + ggtitle("Autocorrelation Function (ACF)")
autoplot(pacf_result) + ggtitle("Partial Autocorrelation Function (PACF)")


### Step5 Generating new features

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)

# Check the regularity of the time series
VAI_ts <- fish_ts_imp |>
  tk_summary_diagnostics(.date_var = date)
view(VAI_ts)

## 5.1 Calendar-based features

fish_ts_imp2 <- rename(fish_ts_imp,DENSITY=value,DATE=date)

VAI_ts_features_Cal <- fish_ts_imp2 |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  # Add Calendar-based (or signature) features
  tk_augment_timeseries_signature(.date_var = DATE) |>
  glimpse()

VAI_ts_features_Cal

# Perform linear regression    
VAI_density_linear_cal <- timetk::plot_time_series_regression(.date_var = date,
                                                              .data = VAI_ts_features_Cal,
                                                              .formula = value ~ as.numeric(date) + index.num
                                                              + year + half + quarter + month + month.lbl,
                                                              .show_summary = TRUE)
summary(VAI_density_linear_cal)


## 5.2 Fourier terms features

# Initialize empty data frame to store results
VAI_ts_features_all <- data.frame()

# Loop through different periods and add Fourier features
for (period in 1:5) {
  augmented_data <- fish_ts_imp2 |>
    # Variance reduction with Log(x+1)
    mutate(DENSITY = log1p(x = DENSITY)) |>
    # Standardization
    mutate(DENSITY = standardize_vec(DENSITY)) |>
    # Add Fourier features for current period
    tk_augment_fourier(.date_var = DATE, .periods = period, .K = 1)
  
  # Append the augmented data to the combined data frame
  VAI_ts_features_all <- bind_rows(VAI_ts_features_all, augmented_data)
}

# Perform linear regression for each combination
VAI_ts_features_all$period <- rownames(VAI_ts_features_all) # Add period column
VAI_ts_features_all$DATE_sin <- paste0("DATE_sin", VAI_ts_features_all$period, "_K1")
VAI_ts_features_all$DATE_cos <- paste0("DATE_cos", VAI_ts_features_all$period, "_K1")

VAI_ts_features_all$fit <- purrr::map2_dbl(
  VAI_ts_features_all$DATE_sin,
  VAI_ts_features_all$DATE_cos,
  ~lm(DENSITY ~ as.numeric(DATE) + .x + .y, data = VAI_ts_features_all)$coef[2:3]
)

# Plot all regressions
plot_time_series_regression(.date_var = DATE,
                            .data = VAI_ts_features_all,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              DATE_sin + DATE_cos,
                            .show_summary = TRUE,
                            .color = "period")


# Initialize empty data frame to store evaluation metrics
VAI_ts_evaluations <- data.frame(periods = integer(0), 
                                 r_squared = double(0), 
                                 AIC = double(0))

# Loop through different periods
for (period in 1:5) {
  augmented_data <- fish_ts_imp2 |>
    # Variance reduction with Log(x+1)
    mutate(DENSITY = log1p(x = DENSITY)) |>
    # Standardization
    mutate(DENSITY = standardize_vec(DENSITY)) |>
    # Add Fourier features for current period
    tk_augment_fourier(.date_var = DATE, .periods = period, .K = 1)
  
  # Append the augmented data to the combined data frame
  VAI_ts_features_all <- bind_rows(VAI_ts_features_all, augmented_data)
  
  # Perform linear regression for the current period
  fit <- lm(DENSITY ~ as.numeric(DATE) + DATE_sin + DATE_cos, data = augmented_data)
  
  # Extract R² and AIC
  r_squared <- summary(fit)$r.squared
  AIC_value <- AIC(fit)
  
  # Store evaluation metrics in the data frame
  VAI_ts_evaluations <- rbind(VAI_ts_evaluations, data.frame(periods = period, 
                                                             r_squared = r_squared, 
                                                             AIC = AIC_value))
}

# Find the best periods based on R² or AIC
best_period_r_squared <- VAI_ts_evaluations |>
  arrange(desc(r_squared)) |>
  slice(1) |>
  pull(periods)

best_period_AIC <- VAI_ts_evaluations |>
  arrange(AIC) |>
  slice(1) |>
  pull(periods)

# Print the best periods
cat("The best period based on R² is:", best_period_r_squared, "\n")
cat("The best period based on AIC is:", best_period_AIC, "\n")


## 5.3 Lag features
library(timekit)
library(tidyr)
library(purrr)
library(ggplot2)

# Define the range of lags you want to use
lags_range <- 1:7

# Create a new data frame with all lags
VAI_ts_features_all_lags <- fish_ts_imp2 |>
  # Variance reduction with Log(x+1)
  mutate(DENSITY = log1p(x = DENSITY)) |>
  # Standardization
  mutate(DENSITY = standardize_vec(DENSITY))

# Add all lag features
for (lag in lags_range) {
  VAI_ts_features_all_lags <- VAI_ts_features_all_lags |>
    tk_augment_lags(.value = DENSITY, .lags = lag)
}

# Perform linear regression for each lag and store coefficients
VAI_ts_features_all_lags <- VAI_ts_features_all_lags |>
  mutate(
    across(starts_with("DENSITY_lag"),
           ~lm(DENSITY ~ as.numeric(DATE) + .x, data = .) %>% 
             broom::tidy() %>% 
             select(term = "DENSITY_lag", estimate = estimate) %>% 
             pull(estimate),
           .names = "{.col}_coeff")
  )

# Prepare data for plotting
VAI_ts_features_plot <- VAI_ts_features_all_lags |>
  pivot_longer(cols = starts_with("DENSITY_lag"),
               names_to = c(".value", "lag"),
               names_pattern = "DENSITY_lag(.*)_coeff",
               values_transform = list(coeff = as.numeric))

# Plot the regression lines for each lag
ggplot(VAI_ts_features_plot, aes(x = DATE, y = DENSITY, group = lag)) +
  geom_line(aes(color = lag, linetype = lag), size = 1) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = viridis::viridis_pal(option = "D")(length(lags_range))) +
  labs(
    title = "Linear Regression Curves for Different Lags",
    x = "DATE",
    y = "DENSITY",
    color = "Lag",
    linetype = "Lag"
  ) +
  theme_bw()

# Perform linear regression for each lag and calculate R²
VAI_ts_features_all_lags <- VAI_ts_features_all_lags |>
  mutate(
    across(starts_with("DENSITY_lag"),
           ~lm(DENSITY ~ as.numeric(DATE) + .x, data = .) %>% 
             broom::glance() %>% 
             select(r.squared = r.squared),
           .names = "{.col}_r_squared")
  )

# Calculate R² for each lag
best_lag <- VAI_ts_features_all_lags |>
  transmute(
    lag = parse_number(str_sub(name, 11)),
    r_squared = across(starts_with("DENSITY_lag_r_squared"), first)
  ) |>
  arrange(desc(r_squared)) |>
  slice(1) |>
  pull(lag)

# Print the best lag
cat("The best lag for the model is:", best_lag)


## 5.4 Moving window statistics
#Note discussed with Xin Ming--Taking the average of x1, x2, x3, ... using a sliding window means performing
#a linear combination on x1, x2, x3, .... However,the purpose of constructing the model 
#is to eliminate the correlation between the independent variables x. 
#Therefore, in terms of parameter settings, to avoid poor model performance
#caused by the correlation of independent variables due to averaging, 
#an algorithm that takes the maximum value is adopted.

VAI_ts_features_Mvwin <- fish_ts_imp2 |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  # Measure Transformation: standardization
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  tk_augment_lags(.value = DENSITY, .lags = c(4, 7)) |>
  # Add moving window statistics
  tk_augment_slidify(.value   = contains("DENSITY"),
                     .f       = ~ max(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

VAI_ts_features_Mvwin

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = VAI_ts_features_Mvwin,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              DENSITY_roll_3 + DENSITY_roll_6,
                            .show_summary = TRUE)


## 5.5 Put all features together 

VAI_ts_features_all <- fish_ts_imp2 |>
  mutate(DENSITY =  log1p(x = DENSITY)) |>
  mutate(DENSITY =  standardize_vec(DENSITY)) |>
  # Add Calendar-based (or signature) features
  tk_augment_timeseries_signature(.date_var = DATE) |>
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  # dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  # Add Fourier features
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>
  # Add lag features
  tk_augment_lags(.value = DENSITY, .lags = c(4,7)) |>
  # Add moving window statistics
  tk_augment_slidify(.value   = contains("DENSITY"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")

VAI_ts_features_all |>
  glimpse()

plot_time_series_regression(.date_var = DATE, 
                            .data = VAI_ts_features_all,
                            .formula = DENSITY ~ as.numeric(DATE) + 
                              index.num + year + half + quarter + month + 
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              DENSITY_roll_3 + DENSITY_roll_6,
                            .show_summary = TRUE)


### Step6 Machine learning for time series learning on class
## 6.1 Load necessary libraries for time series analysis and visualization

library(tidyverse)  
library(timetk) 
library(tidymodels)
library(modeltime)
library(timetk)

# Read data and convert it into a tidy format
VERCah_VAI_ts <- VERCah_VAI |>
  # Convert to a tibble (tidy data format)
  tk_tbl() |>
  # Keep only the date and target variable
  select(index, DATE, DENSITY)

# Visualize the time series data
VERCah_VAI_ml_ts |>
  plot_time_series(DATE, DENSITY,
                   .facet_ncol  = NULL,  # No faceting by columns
                   .smooth      = TRUE,  # Apply smoothing for better visualization
                   .interactive = TRUE,  # Enable interactive plot
                   .title = "DENSITY timeseries") 

library(tidyquant) 

# Create a simple line plot using ggplot2
ggplot(biomtk_ts, aes(x = DATE, y = VERCah_VAI)) +
  geom_line() +  # Draw a line connecting the points
  ggtitle("Density of VAI in Doubs")  


## 6.2 Train/Test Splitting and creating features

n_rows <- nrow(VERCah_VAI_ts)
train_rows <- round(0.7 * n_rows)

train_fish <- VERCah_VAI_ts |>
  slice(1:train_rows) # slice() from dplyr
test_fish <- VERCah_VAI_ts |>
  slice((train_rows):n_rows)


ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = VERCah_VAI_ts, color = "Train"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = VERCah_VAI_ts, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "DENSITY") +
  theme_minimal()

# creating features with recipes

library(recipes)
library(tidymodels)

recipe_spec_final <- recipe(DENSITY ~ ., train_fish) |>
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(recipe_spec_final))

## 6.3 training and evaluating models
# a) Training a boosted tree model

# Workflow
bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

bt

# evaluating model performance

bt_test <- bt |> 
  predict(test_fish) |>
  bind_cols(test_fish) 

bt_test

pbt <- ggplot() +
  geom_line(data = train_fish, 
            aes(x = DATE, y = DENSITY, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = DENSITY, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "DENSITY") +
  theme_minimal()


# Calculating forecast error
bt_test |>
  metrics(VERCah_VAI_ts, .pred)

# b) training a random forest model

rf <- workflow() |>
  add_model(
    spec = rand_forest("regression") |> set_engine("ranger")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

rf

# evaluating model performance

rf_test <- rf |> 
  predict(test_data) |>
  bind_cols(test_data) 

rf_test

prf <- ggplot() +
  geom_line(data = train_fish, 
            aes(x = DATE, y = DENSITY, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = DENSITY, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "DENSITY") +
  theme_minimal()


# Calculating forecast error
rf_test |>
  metrics(DENSITY, .pred)

library(patchwork)
pbt + prf

## 6.4 Comparing among different algorithms
# create a Modeltime Table

model_tbl <- modeltime_table(
  bt,
  rf
)

model_tbl

# Calibration table

calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_fish)

calibrated_tbl 

# Model Evaluation

calibrated_tbl |>
  modeltime_accuracy(test_fish) |>
  arrange(rmse)

# Forecast Plot

calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_fish,
    actual_data = VERCah_VAI_ts,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )

## 6.5 Save the work

workflow_Doubs <- list(
  
  workflows = list(
    
    wflw_random_forest = rf,
    wflw_xgboost = bt
    
  ),
  
  calibration = list(calibration_tbl = calibrated_tbl)
  
)

workflow_Doubs |>
  write_rds("D:/中科大研一第二学期/数据驱动生态学/workflows_Doubs_list.rds")

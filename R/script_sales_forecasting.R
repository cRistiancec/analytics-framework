# FORECASTING SCRIPT ----

# GOAL: Create 8 week forecast horizon by product category ----


# SETUP ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)
library(RabbitTidy)
library(tidymodels)
library(rules)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
library(tictoc)

# * Load Data ----
liquor_tbl_raw <- read_csv("Data/Iowa_Liquor_Sales.csv") %>% 
    as_tibble() %>% 
    clean_names()

liquor_tbl_raw %>% glimpse()

# * Data Filtering & Formatting ----
diageo_americas_tbl <- liquor_tbl_raw %>% 
    filter(vendor_name == "DIAGEO AMERICAS") %>% 
    select(invoice_item_number, date, store_number, store_name, item_number, item_description,
           category_name, volume_sold_gallons, sale_dollars) %>% 
    mutate(date = mdy(date)) %>% 
    filter(date >= as.Date("2016-01-01")) %>% 
    mutate(category = case_when(
        str_detect(category_name, paste(c("Whiskies", "Whiskey"), collapse = "|")) ~ "Whiskey",
        str_detect(category_name, paste(c("Rum"), collapse = "|")) ~ "Rum",
        str_detect(category_name, paste(c("Vodka", "Vodkas"), collapse = "|")) ~ "Vodka",
        str_detect(category_name, paste(c("Gins", "Gin"), collapse = "|")) ~ "Gin",
        str_detect(category_name, paste(c("Scotch"), collapse = "|")) ~ "Scotch",
        str_detect(category_name, paste(c("Tequila"), collapse = "|")) ~ "Tequila",
        str_detect(category_name, paste(c("Liqueurs", "Liqueur"), collapse = "|")) ~ "Liqueur",
        str_detect(category_name, paste(c("Schnapps"), collapse = "|")) ~ "Schnapps",
        str_detect(item_description, "HA Orphan Barrel") ~ "Scotch",
        TRUE ~ "Other"
    )) %>% 
    mutate(sale_dollars = sale_dollars %>% str_remove_all("\\$")) %>% 
    mutate(sale_dollars = as.numeric(sale_dollars))
    
diageo_americas_tbl %>% glimpse()

min(diageo_americas_tbl$date)

max(diageo_americas_tbl$date)

diageo_americas_tbl %>% rt_check_nulls()

# * Save Forecasting Data ----
# diageo_americas_tbl %>% write_rds("Artifacts/diageo_americas_data_forecasting.rds")


# EXPLORATORY DATA ANALYSIS ----

# * Sales by Categories ----
sales_by_category_tbl <- diageo_americas_tbl %>% 
    group_by(category) %>% 
    summarise(gallons_sold = sum(volume_sold_gallons),
              sale_dollars = sum(sale_dollars)) %>% 
    arrange(desc(sale_dollars))

# * Remove "Other" Category
diageo_americas_fcast_tbl <- diageo_americas_tbl %>% 
    filter(category != "Other")


# *  Sales Trends ----

# * Gallons Sold (Daily)
diageo_americas_fcast_tbl %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    plot_time_series(date, gallons_sold, .title = "Gallons Sold Trend (Daily)")

diageo_americas_fcast_tbl %>% 
    group_by(category) %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    plot_time_series(date, gallons_sold, .facet_ncol = 2, .title = "Gallons Sold Trend (Daily)")

# * Gallons Sold (Weekly)
diageo_americas_fcast_tbl %>% 
    summarise_by_time(date, "week", gallons_sold = sum(volume_sold_gallons)) %>% 
    filter(date != as.Date("2017-10-29")) %>% 
    plot_time_series(date, gallons_sold, .title = "Gallons Sold Trend (Weekly)")

diageo_americas_fcast_tbl %>% 
    group_by(category) %>% 
    summarise_by_time(date, "week", gallons_sold = sum(volume_sold_gallons)) %>% 
    filter(date != as.Date("2017-10-29")) %>% 
    plot_time_series(date, gallons_sold, .facet_ncol = 2, .title = "Gallons Sold Trend (Weekly)")

diageo_americas_weekly_tbl <- diageo_americas_fcast_tbl %>% 
    group_by(category) %>% 
    summarise_by_time(
        .date_var    = date,
        .by          = "week",
        gallons_sold = sum(volume_sold_gallons)
    ) %>% 
    ungroup() %>% 
    filter(date != as.Date("2017-10-29")) %>% 
    mutate(category = category %>% fct_relevel(
        "Whiskey", "Vodka", "Rum", "Liqueur", "Gin", "Schnapps", "Tequila", "Scotch"
    ))


# *  Trend Diagnostics ----

# ACF & PACF Plots 

diageo_americas_weekly_tbl %>% 
    filter(category %in% c("Whiskey", "Vodka", "Rum", "Liqueur")) %>% 
    group_by(category) %>% 
    plot_acf_diagnostics(.date_var = date, .value = log(gallons_sold))

diageo_americas_weekly_tbl %>% 
    filter(category %in% c("Gin", "Schnapps", "Tequila", "Scotch")) %>% 
    group_by(category) %>% 
    plot_acf_diagnostics(.date_var = date, .value = log(gallons_sold))


# Anomaly Diagnostics
diageo_americas_weekly_tbl %>% 
    filter(category %in% c("Whiskey", "Vodka", "Rum", "Liqueur")) %>% 
    group_by(category) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = log(gallons_sold), .facet_ncol = 2)

diageo_americas_weekly_tbl %>% 
    filter(category %in% c("Gin", "Schnapps", "Tequila", "Scotch")) %>% 
    group_by(category) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = log(gallons_sold), .facet_ncol = 2)

# Seasonal Diagnostics
diageo_americas_weekly_tbl %>% 
    plot_seasonal_diagnostics(.date_var = date, .value = log(gallons_sold))

diageo_americas_weekly_tbl %>% 
    filter(category == "Gin") %>% 
    plot_seasonal_diagnostics(.date_var = date, .value = log(gallons_sold))



# DATA PREPARATION ----

# * Group Transformations ----

FORECAST_HORIZON <- 12

full_data_tbl <- diageo_americas_weekly_tbl %>% 
    select(date, category, gallons_sold) %>% 
    filter(date != as.Date("2017-10-29")) %>% 
    
    # Fix Data Issues
    group_by(category) %>% 
    pad_by_time(date, .by = "week", .pad_value = 0) %>% 
    ungroup() %>% 
    
    # Transformations (Log)
    mutate(gallons_sold = log(gallons_sold)) %>% 
    
    # Group-Wise Feature Transformations
    group_by(category) %>% 
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>% 
    ungroup() %>% 
    
    # Lags & Rolling Features / Fourier Features
    mutate(category = as_factor(category)) %>% 
    group_by(category) %>% 
    group_split() %>% 
    map(.f = function(df){
        df %>% 
            arrange(date) %>% 
            tk_augment_fourier(date, .periods = c(8, 14, 21)) %>% 
            tk_augment_lags(gallons_sold, .lags = FORECAST_HORIZON) %>% 
            tk_augment_slidify(
                gallons_sold_lag12, 
                .f = ~ mean(.x, na.rm = TRUE),
                .period  = c(8, 14, 21),
                .partial = TRUE,
                .align   = "center"
            )
        
    }) %>% 
    bind_rows() %>% 
    rowid_to_column(var = "row_id")

full_data_tbl %>% glimpse()    

# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(! is.na(gallons_sold)) %>% 
    drop_na()

data_prepared_tbl %>% View()

# * Future Data ----
future_tbl <- full_data_tbl %>% 
    filter(is.na(gallons_sold))

future_tbl %>% View()


# TIME SPLIT ----
splits <- data_prepared_tbl %>% 
    time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, gallons_sold)

train_tbl <- training(splits)
test_tbl  <- testing(splits)


# RECIPE ----

# * Clean Training Set ----

# I skip this step as I will be modeling with outliers. Removing outliers requires more business
# understanding. These outliers might be important for predictions.


# * Recipe Specification ----
recipe_spec <- recipe(gallons_sold ~., data = train_tbl) %>% 
    update_role(row_id, new_role = "indicator") %>% 
    step_timeseries_signature(date) %>% 
    step_rm(matches("(.xts)|(.iso)|(hour)|(minute)|(second)|(am.pm)|(wday)|(mday)|(day)")) %>% 
    step_normalize(date_index.num, date_year) %>% 
    step_other(category) %>% 
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()


# MODELING ----

# * Prophet ----
wflw_fit_prophet <- workflow() %>% 
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(
        spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Prophet Boost ----
wflw_fit_prophet_boost <- workflow() %>% 
    add_model(
        spec = prophet_boost(
            seasonality_daily  = FALSE,
            seasonality_weekly = FALSE,
            seasonality_yearly = FALSE
        ) %>% set_engine("prophet_xgboost")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * SVM ----
wflw_fit_svm_rfb <- workflow() %>% 
    add_model(
        spec = svm_rbf() %>% set_mode("regression") %>% set_engine("kernlab")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Random Forest ----
wflw_fit_ranger <- workflow() %>% 
    add_model(
        spec = rand_forest() %>% set_mode("regression") %>% set_engine("ranger")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Neural Network ----
wflw_fit_nnet <- workflow() %>% 
    add_model(
        spec = mlp() %>% set_mode("regression") %>% set_engine("nnet")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Neural Network ----
wflw_fit_mars <- workflow() %>% 
    add_model(
        spec = mars() %>% set_mode("regression") %>% set_engine("earth")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Cubist Model ----
wflw_fit_cubsist <- workflow() %>% 
    add_model(
        spec = cubist_rules("regression") %>% set_engine("Cubist")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * Accuracy Check ----
submodels_1_tbl <- modeltime_table(
    wflw_fit_prophet,
    wflw_fit_xgboost,
    wflw_fit_prophet_boost, 
    wflw_fit_svm_rfb,
    wflw_fit_ranger,
    wflw_fit_nnet,
    wflw_fit_mars,
    wflw_fit_cubsist
)

submodels_1_tbl %>% 
    modeltime_accuracy(train_tbl) %>% 
    arrange(rmse)


# HYPER-PARAMETER TUNING ----

# * Resamples - K-Fold ----
set.seed(123)
resamples_kfold <- train_tbl %>% vfold_cv(v = 5)

resamples_kfold %>%
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, gallons_sold, .facet_ncol = 2)

# * Xgboost Tune ----

# ** Spec ----
model_spec_xgboost_tune <- boost_tree(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
) %>% 
    set_engine("xgboost")

# ** Workflow ----
wflw_spec_xgboost_tune <- workflow() %>% 
    add_model(model_spec_xgboost_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning ----
tic()
set.seed(123)
tune_results_xgboost <- wflw_spec_xgboost_tune %>% 
    tune_grid(
        resamples  = resamples_kfold,
        param_info = parameters(wflw_spec_xgboost_tune) %>% 
            update(learn_rate = learn_rate(c(0.001, 0.400), trans = NULL)),
        grid       = 10,
        control    = control_grid(verbose = TRUE, allow_par = TRUE)
        
)
toc()

# ** Results ----
tune_results_xgboost %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_xgboost_tuned <- wflw_spec_xgboost_tune %>% 
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>% 
    fit(train_tbl)


# * Ranger Tune ----
# ** Spec ----
model_spec_ranger_tune <- rand_forest(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune()
) %>% 
    set_engine("ranger")

# ** Workflow ----
wflw_spec_ranger_tune <- workflow() %>% 
    add_model(model_spec_ranger_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning ----
tic()
set.seed(123)
tune_results_ranger <- wflw_spec_ranger_tune %>% 
    tune_grid(
        resamples  = resamples_kfold,
        grid       = 10,
        control    = control_grid(verbose = TRUE, allow_par = TRUE)
        
)
toc()

# ** Results ----
tune_results_ranger %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_ranger_tuned <- wflw_spec_ranger_tune %>% 
    finalize_workflow(select_best(tune_results_ranger, "rmse")) %>% 
    fit(train_tbl)


# SVM Tune ----

# ** Spec ----
model_spec_svm_rbf_tune <- svm_rbf(
    mode      = "regression",
    cost      = tune(),
    rbf_sigma = tune(),
    margin    = tune()
) %>% 
    set_engine("kernlab")

# ** Workflow ----
wflw_spec_svm_rbf_tune <- workflow() %>% 
    add_model(model_spec_svm_rbf_tune) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning ----
tic()
set.seed(123)
tune_results_svm_rbf <- wflw_spec_svm_rbf_tune %>% 
    tune_grid(
        resamples = resamples_kfold,
        grid      = 10,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
)
toc()

# ** Results ----
tune_results_svm_rbf %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_svm_rbf_tuned <- wflw_spec_svm_rbf_tune %>% 
    finalize_workflow(select_best(tune_results_svm_rbf, "rmse")) %>% 
    fit(train_tbl)


# Cubist Tune ----

# ** Spec ----
model_spec_cubist_tuned <- cubist_rules(
    mode = "regression",
    committees = tune(),
    neighbors = tune()
) %>% 
    set_engine("Cubist")

# ** Workflow ----
wflw_spec_cubist_tune <- workflow() %>% 
    add_model(model_spec_cubist_tuned) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning ----
tic()
set.seed(123)
tune_results_cubist <- wflw_spec_cubist_tune %>% 
    tune_grid(
        resamples = resamples_kfold,
        grid      = 10,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

# ** Results ----
tune_results_cubist %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_cubist_tuned <- wflw_spec_cubist_tune %>% 
    finalize_workflow(select_best(tune_results_cubist, "rmse")) %>% 
    fit(train_tbl)


# Earth Tune ----
# ** Spec ----
model_spec_earth_tuned <- mars(
    mode        = "regression",
    num_terms   = tune(),
    prod_degree =  tune()
) %>% 
    set_engine("earth")

# ** Workflow ----
wflw_spec_earth_tune <- workflow() %>% 
    add_model(model_spec_earth_tuned) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# ** Tuning ----
tic()
set.seed(123)
tune_results_earth <- wflw_spec_earth_tune %>% 
    tune_grid(
        resamples = resamples_kfold,
        grid      = 10,
        control   = control_grid(verbose = TRUE, allow_par = TRUE)
    )
toc()

# ** Results ----
tune_results_earth %>% show_best("rmse", n = 5)

# ** Finalize ----
wflw_fit_earth_tuned <- wflw_spec_earth_tune %>% 
    finalize_workflow(select_best(tune_results_earth, "rmse")) %>% 
    fit(train_tbl)


# EVALUATE PANEL FORECASTS ----

# * Modeltime Table ----
submodels_2_tbl <- modeltime_table(
    wflw_fit_xgboost_tuned,
    wflw_fit_ranger_tuned,
    wflw_fit_svm_rbf_tuned, 
    wflw_fit_cubist_tuned,
    wflw_fit_earth_tuned
) %>% 
    update_model_description(1, "XGBOOST-Tuned") %>% 
    update_model_description(2, "RANGER-Tuned") %>% 
    update_model_description(3, "KERNLAB-Tuned") %>% 
    update_model_description(4, "CUBIST-Tuned") %>% 
    update_model_description(5, "EARTH-Tuned") %>% 
    combine_modeltime_tables(submodels_1_tbl)

# * Calibration ----
calibration_2_tbl <- submodels_2_tbl %>% 
    modeltime_calibrate(test_tbl)

# * Accuracy ----
accuracy_2_tbl <- calibration_2_tbl %>% 
    modeltime_accuracy() %>% 
    arrange(rmse)

# * Forecast Test Visualization ----
calibration_2_tbl %>% 
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>% 
    group_by(category) %>% 
    plot_modeltime_forecast(
        .facet_ncol          = 2,
        .conf_interval_alpha = 0.1,
        .interactive         = TRUE
    )


# RESAMPLING ----
# - Assess the stability of our models over time
# - Helps us strategize an ensemble approach

# * Time Series CV ----
resamples_tscv <- train_tbl %>% 
    time_series_cv(
        assess      = FORECAST_HORIZON,
        skip        = FORECAST_HORIZON,
        cumulative  = TRUE,
        slice_limit = 4
    )

resamples_tscv %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, gallons_sold)

# * Fitting Resamples ----
model_tbl_tuned_resamples <- submodels_2_tbl %>% 
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(verbose = TRUE, allow_par = TRUE) 
    )

# * Resampling Accuracy Table ----
model_tbl_tuned_resamples %>% 
    modeltime_resample_accuracy(
        metric_set = metric_set(rmse),
        summary_fns = list(mean = mean, sd = sd)
    ) %>% 
    arrange(rmse_mean)

# * Resampling Accuracy Plot ----
model_tbl_tuned_resamples %>% 
    plot_modeltime_resamples(
        .metric_set = metric_set(mae, rmse, rsq),
        .point_size = 4,
        .point_alpha = 0.8,
        .facet_ncol = 1
    )


# ENSEMBLE MODELS ----

# * Average Ensemble ----
submodels_2_ids_to_keep <- c(10, 2)

ensemble_fit <- submodels_2_tbl %>% 
    filter(.model_id %in% submodels_2_ids_to_keep) %>% 
    ensemble_average()

model_ensemble_tbl <- modeltime_table(ensemble_fit)

# * Accuracy ----
model_ensemble_tbl %>% 
    modeltime_accuracy(test_tbl)

# * Forecast ----
forecast_ensemble_test_tbl <- model_ensemble_tbl %>% 
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>% 
    mutate(across(.cols = c(.value, gallons_sold), .fns = exp))

forecast_ensemble_test_tbl %>% 
    group_by(category) %>% 
    plot_modeltime_forecast(.facet_ncol = 2, .conf_interval_alpha = 0.1)


forecast_ensemble_test_tbl %>% 
    filter(.key == "prediction") %>% 
    select(category, .value, gallons_sold) %>% 
    group_by(category) %>% 
    summarize_accuracy_metrics(
        truth      = gallons_sold,
        estimate   = .value,
        metric_set = metric_set(mae, rmse, rsq)
    )


# Refit ----
model_ensemble_refit_tbl <- model_ensemble_tbl %>% 
    modeltime_refit(data_prepared_tbl)

model_ensemble_refit_tbl %>% 
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data = TRUE
    ) %>% 
    mutate(
        .value = exp(.value),
        gallons_sold = exp(gallons_sold)
    ) %>% 
    group_by(category) %>% 
    plot_modeltime_forecast(
        .facet_ncol = 2,
        .y_intercept = 0,
        .conf_interval_alpha = 0.1,
        .legend_show = FALSE,
        .interactive = FALSE
    )

# SAVING ARTIFACTS ----

data_prepared_tbl

feature_engineering_artifacts_list_1 <- list(
    
    # Data
    data = list(
        data_prepared_tbl = data_prepared_tbl,
        forecast_tbl      = future_tbl,
        train_tbl         = train_tbl,
        test_tbl          = test_tbl
    ),
    
    # Recipes
    recipes = list(recipe  = recipe_spec),
    
    # Model Tables
    model_tables = list(
        model_table       = submodels_2_tbl,
        ensemble_table    = model_ensemble_tbl
    )
)

feature_engineering_artifacts_list_1 %>% 
    write_rds("Artifacts/feature_engineering_artifacts_list_1.rds")


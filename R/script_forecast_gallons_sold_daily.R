# DAILY FORECAST SCRIPT (DAILY) ----

# SAVING PATHS ----
# screenshot_path <- "../LucasO-Blogdown/content/project/Forecasting-For-Liquor-Sales/Screenshots/"

# write_rds(paste0(screenshot_path,""))


# 1.0: SETUP ----

# * 1.1: Libraries ----
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

# * 1.2: Load Data ----
diageo_americas_tbl <- read_rds("Artifacts/diageo_americas_data_forecasting.rds") %>% 
    filter(category != "Other")

diageo_americas_tbl

min(diageo_americas_tbl$date)
max(diageo_americas_tbl$date)


# 2.0: EXPLORATORY DATA ANALYSIS ----

color <- "#2c3e50"

# * 2.1: Sales Trends ----

# ** Gallons Sold Trend ----
gallons_sold_trend_plot <- diageo_americas_tbl %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    plot_time_series(
        date, 
        gallons_sold, 
        .title = "Gallons Sold Trend (Daily)",
        .interactive = FALSE
    )

gallons_sold_trend_plot %>% write_rds(paste0(screenshot_path,"gallons_sold_trend_plot.rds"))

# ** Gallons Sold by Category Trend ----
gallons_sold_by_category_trend_plot <- diageo_americas_tbl %>% 
    group_by(category) %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    plot_time_series(
        date, 
        gallons_sold, 
        .title = "Gallons Sold by Category Trend (Daily)",
        .interactive = FALSE,
        .facet_ncol = 2
    )

gallons_sold_by_category_trend_plot %>% write_rds(paste0(screenshot_path,"gallons_sold_by_category_trend_plot.rds"))

# ** Gallons Sold by Weekday ----
gallons_sold_by_day <- diageo_americas_tbl %>% 
    mutate(day_of_week = wday(date, label = TRUE)) %>% 
    group_by(day_of_week) %>% 
    summarise(gallons_sold = sum(volume_sold_gallons)) %>% 
    ungroup() %>% 
    mutate(label_txt = gallons_sold %>% scales::comma(scale = 1e-3, accuracy = .1, suffix = "K"))

gallons_sold_by_day_plot <- gallons_sold_by_day %>% 
    mutate(day_of_week = day_of_week %>% fct_rev()) %>% 
    ggplot(aes(gallons_sold, day_of_week))+
    geom_col(fill = color)+
    geom_text(aes(label = label_txt), hjust = -0.02, fontface = "bold", color = color, size = 3)+
    scale_x_continuous(labels = scales::comma_format(scale = 1e-3, accuracy = .1, suffix = "K"))+
    tidyquant::theme_tq()+
    labs(title = "Gallons Sold by Day of Week", x = "gallons sold (thousands)", y = NULL)+
    theme(axis.title = element_text(size = 8))
    
gallons_sold_by_day_plot %>% write_rds(paste0(screenshot_path,"gallons_sold_by_wday.rds"))
    
    
# * 2.2: Trend Diagnostics ----

# ** Summarise Gallons Sold by Time (Daily) ----
daily_category_tbl <- diageo_americas_tbl %>% 
    group_by(category) %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    ungroup() %>% 
    mutate(category = category %>% fct_relevel(
        "Whiskey", "Vodka", "Rum", "Liqueur", "Gin", "Schnapps", "Tequila", "Scotch"
    ))

daily_tbl <- diageo_americas_tbl %>% 
    summarise_by_time(date, "day", gallons_sold = sum(volume_sold_gallons)) %>% 
    ungroup()


# ** ACF & PACF Plots ----
daily_tbl %>% 
    plot_acf_diagnostics(.date_var = date, .value = log(gallons_sold))

# ** Anomaly Diagnostics ----
daily_category_tbl %>% 
    filter(category %in% c("Whiskey", "Vodka", "Rum", "Liqueur")) %>% 
    group_by(category) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = log1p(gallons_sold), .facet_ncol = 2)

daily_category_tbl %>% 
    filter(category %in% c("Gin", "Schnapps", "Tequila", "Scotch")) %>% 
    group_by(category) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = log1p(gallons_sold), .facet_ncol = 2)

# ** Seasonal Diagnostics ----
daily_category_tbl %>% 
    filter(category == "Whiskey") %>% 
    plot_seasonal_diagnostics(.date_var = date, .value = log1p(gallons_sold))


# 3.0: DATA PREPARATION ----

FORECAST_HORIZON <- 28

# * 3.1: Group Transformations ----
full_data_tbl <- daily_category_tbl %>% 
    select(date, category, gallons_sold) %>% 
    
    # Fix Data Issues
    group_by(category) %>% 
    pad_by_time(date, .by = "day", .pad_value = 0) %>% 
    ungroup() %>% 
    
    # Transformations (Log)
    mutate(gallons_sold = log1p(gallons_sold)) %>% 
    
    # Group-Wise Feature Transformations
    group_by(category) %>% 
    future_frame(date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>% 
    ungroup() %>% 
    mutate(day = wday(date, label = TRUE)) %>% 
    filter(day != "Sun") %>% 
    select(-day) %>% 
    
    # Lags & Rolling Features / Fourier Features
    mutate(category = as_factor(category)) %>% 
    group_by(category) %>% 
    group_split() %>% 
    map(.f = function(df){
        df %>% 
            arrange(date) %>% 
            tk_augment_fourier(date, .periods = c(5, 10, 30)) %>% 
            tk_augment_lags(gallons_sold, .lags = FORECAST_HORIZON) %>% 
            tk_augment_slidify(
                gallons_sold_lag28, 
                .f = ~ mean(.x, na.rm = TRUE),
                .period  = c(5, 10, 30),
                .partial = TRUE,
                .align   = "center"
            )
        
    }) %>% 
    bind_rows() %>% 
    rowid_to_column(var = "row_id")

full_data_tbl %>% glimpse()    

# * 3.2: Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(! is.na(gallons_sold)) %>% 
    drop_na()

# * 3.3: Future Data ----
future_tbl <- full_data_tbl %>% 
    filter(is.na(gallons_sold))


# 4.0: TIME SPLIT ----
splits <- data_prepared_tbl %>% 
    time_series_split(date, assess = FORECAST_HORIZON, cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, gallons_sold)

train_tbl <- training(splits)
test_tbl  <- testing(splits)

# 5.0: RECIPE ----

# * 5.1: Clean Training Set ----

# I skip this step as I will be modeling with outliers. Removing outliers requires more business
# understanding. These outliers might be important for predictions.


# * 5.2: Recipe Specification ----
recipe_spec <- recipe(gallons_sold ~., data = train_tbl) %>% 
    update_role(row_id, new_role = "indicator") %>% 
    step_timeseries_signature(date) %>% 
    step_rm(matches("(.xts)|(.iso)|(hour)|(minute)|(second)|(am.pm)")) %>% 
    step_normalize(date_index.num, date_year) %>% 
    step_other(category) %>% 
    step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()


# 6.0: MODELING ----

# * 6.1: Prophet ----
wflw_fit_prophet <- workflow() %>% 
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * 6.2: Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(
        spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.3: Prophet Boost ----
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

# * 6.4: SVM ----
wflw_fit_svm_rfb <- workflow() %>% 
    add_model(
        spec = svm_rbf() %>% set_mode("regression") %>% set_engine("kernlab")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.5: Random Forest ----
wflw_fit_ranger <- workflow() %>% 
    add_model(
        spec = rand_forest() %>% set_mode("regression") %>% set_engine("ranger")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.6: Neural Network ----
wflw_fit_nnet <- workflow() %>% 
    add_model(
        spec = mlp() %>% set_mode("regression") %>% set_engine("nnet")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.7: Earth Network ----
wflw_fit_mars <- workflow() %>% 
    add_model(
        spec = mars() %>% set_mode("regression") %>% set_engine("earth")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.8: Cubist Model ----
wflw_fit_cubsist <- workflow() %>% 
    add_model(
        spec = cubist_rules("regression") %>% set_engine("Cubist")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(train_tbl)

# * 6.9: Accuracy Check ----
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


# 7.0: HYPER-PARAMETER TUNING ----

# * 7.1: Resamples - K-Fold ----
set.seed(123)
resamples_kfold <- train_tbl %>% vfold_cv(v = 5)

resamples_kfold %>%
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(date, gallons_sold, .facet_ncol = 2)


# * 7.2: Xgboost Tune ----

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


# * 7.3: Ranger Tune ----

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


# * 7.4: SVM Tune ----

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


# * 7.5: Cubist Tune ----

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


# * 7.6: Earth Tune ----

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


# 8.0: EVALUATE PANEL FORECASTS ----

# * 8.1: Modeltime Table ----
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

# * 8.2: Calibration ----
calibration_2_tbl <- submodels_2_tbl %>% 
    modeltime_calibrate(test_tbl)

# * 8.3: Accuracy ----
accuracy_2_tbl <- calibration_2_tbl %>% 
    modeltime_accuracy() %>% 
    arrange(rmse)

# * 8.4: Forecast Test Visualization ----
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


# 9.0: RESAMPLING ----
# - Assess the stability of our models over time
# - Helps us strategize an ensemble approach

# * 9.1: Time Series CV ----
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

# * 9.2: Fitting Resamples ----
model_tbl_tuned_resamples <- submodels_2_tbl %>% 
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(verbose = TRUE, allow_par = TRUE) 
    )

# * 9.3 Resampling Accuracy Table ----
model_tbl_tuned_resamples_accuracy <- model_tbl_tuned_resamples %>% 
    modeltime_resample_accuracy(
        metric_set = metric_set(rmse),
        summary_fns = list(mean = mean, sd = sd)
    ) %>% 
    arrange(rmse_mean)


# * 9.4: Resampling Accuracy Plot ----
model_tbl_tuned_resamples %>% 
    plot_modeltime_resamples(
        .metric_set = metric_set(mae, rmse, rsq),
        .point_size = 4,
        .point_alpha = 0.8,
        .facet_ncol = 1
    )


# 10.0: ENSEMBLE MODELS ----

# * 10.1: Average Ensemble ----
submodels_2_ids_to_keep <- c(5, 2, 4, 1)

ensemble_fit_1 <- submodels_2_tbl %>% 
    filter(.model_id %in% submodels_2_ids_to_keep[1:2]) %>% 
    ensemble_average()

ensemble_fit_2 <- submodels_2_tbl %>% 
    filter(.model_id %in% submodels_2_ids_to_keep) %>% 
    ensemble_average()

ensemble_fit_3 <- submodels_2_tbl %>% 
    filter(.model_id %in% c(5, 2, 3)) %>% 
    ensemble_average()

model_ensemble_tbl <- modeltime_table(
    ensemble_fit_1,
    ensemble_fit_2,
    ensemble_fit_3
)

# * 10.2: Accuracy ----
model_ensemble_calibrate_tbl <- model_ensemble_tbl %>% 
    modeltime_calibrate(test_tbl)

model_ensemble_calibrate_tbl %>% 
    modeltime_accuracy()

# * 10.3: Forecast ----
forecast_ensemble_test_tbl <- model_ensemble_tbl %>% 
    modeltime_forecast(
        new_data    = test_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>% 
    mutate(across(.cols = c(.value, gallons_sold), .fns = expm1))

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

# 10.4: Refit ----
model_ensemble_refit_tbl <- model_ensemble_tbl %>% 
    modeltime_refit(data_prepared_tbl)

model_ensemble_forecast <- model_ensemble_refit_tbl %>% 
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data = TRUE
    ) %>% 
    mutate(
        .value = exp(.value),
        gallons_sold = exp(gallons_sold)
    ) 

# 10.5: Visualize Future Forecast ----
model_ensemble_forecast %>% 
    filter(.model_desc %in% c("ACTUAL", "ENSEMBLE (MEAN): 4 MODELS")) %>% 
    filter(date >= as.Date("2017-05-01")) %>% 
    group_by(category) %>% 
    plot_modeltime_forecast(
        .facet_ncol = 2,
        .y_intercept = 0,
        .conf_interval_alpha = 0.1,
        .legend_show = FALSE,
        .interactive = FALSE,
        .title = "Future Forecast - 4 Model Ensemble"
    ) %>% 
    write_rds(paste0(screenshot_path,"future_forecast_4_model_ensemble.rds"))


# 11.0: SAVING ARTIFACTS ----

feature_engineering_artifacts_list_2 <- list(
    # Data
    data = list(
        data_prepared_tbl = data_prepared_tbl,
        forecast_tbl      = future_tbl,
        train_tbl         = train_tbl,
        test_tbl          = test_tbl
    ),
    
    # Recipe
    recipes = list(recipe = recipe_spec),
    
    # Models Tuned
    models = list(
        sub_models_tuned = calibration_2_tbl,
        ensemble_models  = model_ensemble_calibrate_tbl
    )
    
    
)

feature_engineering_artifacts_list_2 %>% 
    write_rds("Artifacts/feature_engineering_artifacts_list_2.rds")

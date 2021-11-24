# FORECASTING SCRIPT ----

# GOAL: Create 8 week forecast horizon by product category ----


# SETUP ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)
library(RabbitTidy)

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



# FEATURE ENGINEERING ----

# * Group Transformations ----

FORECAST_HORIZON <- 8

diageo_americas_weekly_tbl %>% 
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
    ungroup()
    
    
    
    


# RFM ANALYSIS FOR LIQUOR ----

# DATA SOURCE: https://www.kaggle.com/residentmario/iowa-liquor-sales

# screenshot_path <- "../LucasO-Blogdown/content/project/RFM-Analysis-For-Liquor-Sales/Screenshots/"

# Setup ----

# * Libraries ----
library(tidyverse)
library(janitor)
library(RabbitTidy)
library(lubridate)
library(rfm)
library(tidyquant)

# * Load Data ----
liquor_tbl_raw <- read_csv("Data/Iowa_Liquor_Sales.csv") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(date = mdy(date))

liquor_tbl_raw %>% glimpse()

min(liquor_tbl_raw$date)

max(liquor_tbl_raw$date)

# Prepare Data For Analysis ----
liquor_tbl <- liquor_tbl_raw %>% 
    
    # Filter for most recent year
    filter(date >= as.Date("2017-05-01")) %>% 
    
    # Select columns needed for analysis
    select(invoice_item_number, date, store_number, store_name, vendor_name, 
           item_description, volume_sold_gallons, sale_dollars) %>% 
    
    # Formatting
    mutate(sale_dollars = sale_dollars %>% str_remove_all("\\$")) %>% 
    mutate(sale_dollars = as.numeric(sale_dollars))

liquor_tbl %>% glimpse()

# liquor_tbl %>% write_rds("../LucasO-Blogdown/content/project/RFM-Analysis-For-Liquor-Sales/Data/data.rds")


# Volume (Gallon) Sold By Vendor ----
liquor_tbl %>% 
    group_by(vendor_name) %>% 
    summarise(volume_sold_gallons = sum(volume_sold_gallons)) %>% 
    ungroup() %>% 
    arrange(desc(volume_sold_gallons))

liquor_tbl %>% 
    filter(vendor_name == "DIAGEO AMERICAS") 

diageo_tbl <- liquor_tbl %>% 
    filter(vendor_name == "DIAGEO AMERICAS")

diageo_tbl


ScSc# RFM Analysis ----

# * RFM Table ----
rfm_tbl <- rfm_table_order(
    data          = diageo_tbl, 
    customer_id   = store_name, 
    order_date    = date,
    revenue       = sale_dollars,
    analysis_date = as.Date("2017-10-31")
)

rfm_tbl 

# * Segmenting Customers ----
segment_names <-  c("Champions", "Loyal Customers", "Potential Loyalist",
                  "New Customers", "Promising", "Need Attention", "About To Sleep",
                  "At Risk", "Can't Lose Them", "Lost")

recency_lower   <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper   <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower  <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper  <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)


rfm_segment_tbl <- rfm_segment(
    data            = rfm_tbl,
    segment_names   = segment_names,
    recency_lower   = recency_lower,
    recency_upper   = recency_upper,
    frequency_lower = frequency_lower,
    frequency_upper = frequency_upper,
    monetary_lower  = monetary_lower,
    monetary_upper  = monetary_upper 
    
)

segment_count <- rfm_segment_tbl %>%
    count(segment) %>% 
    mutate(pct = n/sum(n)) %>% 
    arrange(desc(n)) %>% 
    mutate(label_txt = pct %>% scales::percent(accuracy = 1)) %>% 
    mutate(segment = segment %>% fct_reorder(n))

segment_count_plot <- segment_count %>% 
    ggplot(aes(n, segment))+
    geom_col(fill = "#2c3e50")+
    geom_text(aes(label = label_txt), hjust = 1, col = "white", fontface = "bold")+
    theme_tq()+
    labs(title = "Customer Segments", y = NULL, x = NULL) 

segment_count_plot %>% 
    write_rds(paste0(screenshot_path,"segment_prop.rds"))
    

# * Plotting Function ----
func_rfm_segment_plot <- function(data, stat, rfm_var, accuracy, fct_rev = FALSE){
    
    rfm_var_expr <- rlang::enquo(rfm_var)
    rfm_var_name <- rlang::quo_name(rfm_var_expr)
    
    data_tbl <- data %>% 
        group_by(segment) %>% 
        summarise(value = stat(!! rfm_var_expr)) %>% 
        ungroup() %>% 
        mutate(segment = segment %>% fct_reorder(value)) 
    
    if(rfm_var_name == "amount"){
        data_tbl <- data_tbl %>% 
            mutate(value_txt = value %>% scales::dollar(accuracy = accuracy))
    }
    
    if(rfm_var_name == "transaction_count"){
        data_tbl <- data_tbl %>% 
            mutate(value_txt = value %>% scales::comma(accuracy = accuracy))
    }
    
    if(rfm_var_name == "recency_days"){
        data_tbl <- data_tbl %>% 
            mutate(value_txt = value %>% scales::comma(accuracy = accuracy))
    }
    
    if(fct_rev){
        data_tbl <- data_tbl %>% 
            mutate(segment = segment %>% fct_rev())
    }
    
    
    # Plot 
    plot <- data_tbl %>% 
        ggplot(aes(value, segment))+
        geom_col(fill = "#2c3e50")+
        geom_text(aes(label = value_txt), hjust = 1, color = "white", fontface = "bold")+
        theme_tq()

     
    
    return(plot)
    
}

# Plot of Median Recency Value
recency_plot <- func_rfm_segment_plot(
    data     = rfm_segment_tbl,
    stat     = median,
    rfm_var  = recency_days,
    accuracy = 1,
    fct_rev  = TRUE
)+
    labs(title = "Median Recency by Segment", y = NULL, x = "number of days")

recency_plot %>% write_rds(paste0(screenshot_path,"recency_plot.rds"))

# Plot of Median Frequency Value
frequency_plot <- func_rfm_segment_plot(
    data     = rfm_segment_tbl,
    stat     = median,
    rfm_var  = transaction_count,
    accuracy = 1,
    fct_rev  = FALSE
)+
    labs(title = "Median Frequency by Segment", y = NULL, x = "number of transactions")

frequency_plot %>% write_rds(paste0(screenshot_path,"frequency_plot.rds"))

# Plot of Median Monetary Value
monetary_plot <- func_rfm_segment_plot(
    data     = rfm_segment_tbl,
    stat     = median,
    rfm_var  = amount,
    accuracy = 1,
    fct_rev  = FALSE
)+
    scale_x_continuous(labels = scales::dollar_format())+
    labs(title = "Median Monetary Value by Segment", y = NULL, x = "Amount Spent")

monetary_plot %>% write_rds(paste0(screenshot_path,"monetary_plot.rds"))

# Histogram of RFM Values
rfm_histogram <- rfm_histograms(
    rfm_table = rfm_tbl,
    hist_bins = 10,
    hist_color = "#2c3e50"
) + theme_tq()

rfm_histogram %>% write_rds(paste0(screenshot_path,"rfm_histograms.rds"))

# Recency vs Monetary Value
recency_vs_monetary_plot <- rfm_rm_plot(
    rfm_table = rfm_tbl,
    point_color = "#2c3e50"
)+ theme_tq()+
    scale_x_continuous(labels = scales::dollar_format())

recency_vs_monetary_plot %>% write_rds(paste0(screenshot_path,"recency_vs_monetary_plot.rds"))

# Frequency vs Monetary Value
frequency_vs_monetary_plot <- rfm_fm_plot(
    rfm_table = rfm_tbl,
    point_color = "#2c3e50"
)+ theme_tq()+
    scale_x_continuous(labels = scales::dollar_format())

frequency_vs_monetary_plot %>% write_rds(paste0(screenshot_path,"frequency_vs_monetary_plot.rds"))


# Recency vs Frequency
recency_vs_frequency_plot <- rfm_rf_plot(
    rfm_table = rfm_tbl,
    point_color = "#2c3e50"
)+ theme_tq()+
    scale_x_continuous(labels = scales::comma_format())

recency_vs_frequency_plot %>% write_rds(paste0(screenshot_path,"recency_vs_frequency_plot.rds"))
    
    

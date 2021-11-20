# RFM ANALYSIS ----

# DATA SOURCE: https://www.kaggle.com/residentmario/iowa-liquor-sales


# Libraries ----
library(tidyverse)
library(janitor)
library(RabbitTidy)
library(lubridate)
library(rfm)

# Load Data ----
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
    filter(date >= as.Date("2017-01-01")) %>% 
    
    # Select columns needed for analysis
    select(invoice_item_number, date, store_number, store_name, vendor_name, 
           item_description, volume_sold_gallons, sale_dollars) %>% 
    
    # Formatting
    mutate(sale_dollars = sale_dollars %>% str_remove_all("\\$")) %>% 
    mutate(sale_dollars = as.numeric(sale_dollars))

liquor_tbl %>% glimpse()


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

# RFM Analysis ----

# * RFM Table ----
rfm_table <- rfm_table_order(
    diageo_tbl, store_name, date, sale_dollars, as.Date("2017-10-31")
)

# Segmenting Customers ----
segment_names = c("Champions", "Loyal Customers", "Potential Loyalist",
                  "New Customers", "Promising", "Need Attention", "About To Sleep",
                  "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)


rfm_segment_tbl <- rfm_segment(
    data            = rfm_table,
    segment_names   = segment_names,
    recency_lower   = recency_lower,
    recency_upper   = recency_upper,
    frequency_lower = frequency_lower,
    frequency_upper = frequency_upper,
    monetary_lower  = monetary_lower,
    monetary_upper  = monetary_upper 
    
)

rfm_segment_tbl %>%
    count(segment) %>% 
    mutate(pct = n/sum(n)) %>% 
    arrange(desc(n))

rfm_segment_tbl %>% 
    group_by(segment) %>% 
    summarise(median_sales = median(amount)) %>% 
    ungroup() %>% 
    mutate(segment = segment %>% fct_reorder(median_sales)) %>% 
    ggplot(aes(median_sales, segment))+
    geom_col()

# Heatmap
rfm_heatmap(rfm_table)

rfm_bar_chart(rfm_table)+
    theme_bw()



    
    

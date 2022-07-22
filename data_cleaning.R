library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
#solve date

data_name <-
  list('all',
       "art",
       "collectible",
       "game",
       "metaverse",
       "utility")

merge_data <-
  function(data_name) {
    data_type <- list(
      "N_USD_",
      "Buyer_Seller_",
      "Average_Active_",
      "Primary_Secondary_",
      "Primary_Secondary_USD_"
    )
    df_1 = read.csv(paste(data_type[1], data_name, ".csv", sep = ""))
    df_2 = read.csv(paste(data_type[2], data_name, ".csv", sep = ""))
    df_3 = read.csv(paste(data_type[3], data_name, ".csv", sep = ""))
    df_4 = read.csv(paste(data_type[4], data_name, ".csv", sep = ""))
    df_5 = read.csv(paste(data_type[5], data_name, ".csv", sep = ""))
    
    df_1$DateTime = as.Date(df_1$DateTime, format = "%Y-%m-%d")
    df_2$DateTime = as.Date(df_1$DateTime, format = "%Y-%m-%d")
    df_3$DateTime = as.Date(df_1$DateTime, format = "%Y-%m-%d")
    df_4$DateTime = as.Date(df_1$DateTime, format = "%Y-%m-%d")
    df_5$DateTime = as.Date(df_1$DateTime, format = "%Y-%m-%d")
    
    temp_df = df_1 %>%
      inner_join(df_2, by = "DateTime") %>%
      inner_join(df_3, by = "DateTime") %>%
      inner_join(df_4, by = "DateTime") %>%
      inner_join(df_5, by = "DateTime") %>%
      mutate(segment = data_name)
    return(write.csv(temp_df, paste(data_name, ".csv", sep = "")))
  }
for (name in data_name) {
  merge_data(name)
}

ETH = read.csv("eth.csv") %>%
  select(c(1, 6))

data <- read.csv(paste(data_name[1], ".csv", sep = "")) %>%
  full_join(read.csv(paste(data_name[2], ".csv", sep = ""))) %>%
  full_join(read.csv(paste(data_name[3], ".csv", sep = ""))) %>%
  full_join(read.csv(paste(data_name[4], ".csv", sep = ""))) %>%
  full_join(read.csv(paste(data_name[5], ".csv", sep = ""))) %>%
  full_join(read.csv(paste(data_name[6], ".csv", sep = ""))) %>%
  inner_join(ETH, by = c("DateTime" = "Date")) %>%
  select(seq(2, 14, 1)) %>%
  rename(
    "Date" = "DateTime",
    "NumSales" = "Number.of.sales",
    "USDSales" = "Sales.USD",
    "Buyer" = "Unique.buyers" ,
    "Seller" = "Unique.sellers" ,
    "USDAverage" = "Average.USD",
    "Wallets" = "Active.market.wallets",
    "PriSales" = "Primary.Sales",
    "SecSales" = "Secondary.sales",
    "USDPriSales" = "Primary.sales.USD" ,
    "USDSecSales" = "Secondary.sales.USD" ,
    "Seg" = "segment",
    "ETHPrice" = "Adj.Close"
  ) %>%
  na.omit() %>%
  select(c(1,12,seq(2,11,1),13))
write.csv(data,"data.csv")
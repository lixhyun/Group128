library(tidyverse)
library(plotly)

df <- read.csv("data.csv") %>%
  select(seq(2, 14, 1))

df$Date = as.Date(df$Date, format = "%Y-%m-%d")

max_date <- function(df) {
  time = seq(2018, 2022, 1)
  max_day <- df %>%
    filter(as.numeric(format(Date, '%Y')) == 2017) %>%
    group_by(Seg) %>%
    summarise(num_max = max(NumSales), day_max = Date[NumSales == max(NumSales)])
  
  for (year in time) {
    max_day <- df %>%
      filter(as.numeric(format(Date, '%Y')) == year) %>%
      group_by(Seg) %>%
      summarise(num_max = max(NumSales), day_max = Date[NumSales == max(NumSales)]) %>%
      full_join(max_day)
    
  }
  return(max_day)
}

day_max = max_date(df)

df_1_1 <- df %>%
  filter(Seg == c("all", "art")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = NumSales, color = Seg)) +
  theme_classic() +
  labs(
    title = paste("Market Trend of", "Number of Sales"),
    y = "Number of Sales",
    colour = "Segment"
  )
df_1_1 <- df_1_1 +
  geom_point(data = day_max %>%
              filter(Seg == c("all", "art")),
            aes(x = day_max, y = num_max), colour = "black" ,size = 1) + 
  geom_line(data = df %>%
            filter(Seg != c("all", "art")), aes(x = Date, y = NumSales, color = Seg), alpha = 0.1)


ggplotly(df_1_1)

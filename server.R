# Define server 
server <- function(input, output, session) {
  #Load_data
  nnwords <- read.csv("nnwords.csv")
  data <- read.csv("data.csv")
  ETH <- read.csv("eth.csv")
  nftz <- read.csv("nftz.csv")
  nft_tweet <- read.csv("nft_tweets.csv")
  nftz$Date <- as.Date(nftz$Date, format = "%Y-%m-%d")
  ETH$Date <- as.Date(ETH$Date, format = "%Y-%m-%d")
  
  
  #Page1
  output$WordCloud <- renderPlot({
    set.seed(1)
    minFreq = 10
    maxWords = 100
    
    cover <- wordcloud(
      words = nnwords$word,
      freq = nnwords$freq,
      min.freq = minFreq,
      max.words = maxWords,
      random.order = FALSE,
      colors = nnwords$color,
      ordered.colors = TRUE
    )
    return(cover)
  })
  output$slickr <- renderSlickR({
    imgs <- list.files("www", pattern = ".jpg", full.names = TRUE)
    slickR(imgs)
  })
  
  
  #Page2
  ################################################################################1
  output$value <- renderPrint({
    input$show_vars
  })
  output$value <- renderPrint({
    input$action
  })
  output$value <- renderPrint({
    input$action2
  })
  output$value <- renderPrint({
    input$year
  })
  
  
  #GRAPH #1 #Market_Trend
  output$G_Market_Trend <- renderPlotly({
    df <- data %>%
      select(seq(2, 14, 1))
    df$Date = as.Date(df$Date, format = "%Y-%m-%d")
    
    G_Market_trend <- df %>%
      filter(Seg == input$show_vars) %>%
      ggplot() +
      geom_line(aes(x = Date, y = NumSales, color = Seg)) +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(
        title = paste("Number of Sales Reaches a High Level in 2021"),
        y = "Number of Sales",
        colour = "Segment"
      ) +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    
    
    
    ggplotly(G_Market_trend)
  })
  
  #Graph_2
  
  output$G_Buyer_Seller <- renderPlotly({
    temp_df <- data %>%
      select("Date", "Seg",  "Buyer", "Seller") %>%
      filter(Date == as.Date(input$year, format = "%Y-%m-%d")) %>%
      pivot_longer(c("Buyer", "Seller"),
                   names_to = "user",
                   values_to = "number")
    
    G_bar_chart <- temp_df %>%
      ggplot() +
      geom_bar(aes(x = Seg,
                   y = number,
                   fill = user),
               stat = "identity",
               position = position_dodge()) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(
        title = " Unique Buyers and Sellers Focus on Collectible and Game Segment in Current Year",
        Segment = " The Number of Unique Buyer",
        y = "The Number of Unique Buyer/Seller",
        x = "Segment",
        fill = "Trader"
      ) +
      annotate(
        "text",
        x = "metaverse",
        y = 0.95 * max(temp_df$number),
        size = 12,
        colour = rgb(0, 0, 0, 0.5),
        label = input$year
      )
    
    ggplotly(G_bar_chart)
  })
  
  #Graph 3&4
  output$G_Wallets = renderPlotly({
    tempdf <- data %>%
      select("Date", "Seg",  "Wallets")
    tempdf$Date <- as.Date(tempdf$Date, format = "%Y-%m-%d")
    
    G_Wallets <- tempdf %>%
      ggplot() +
      geom_area((aes(
        x = Date, y = Wallets, fill = Seg
      ))) +
      labs(title = "Active Wallets are Mainly Compostied of Game and Other Segment from 2022",
           y = "The Number of Wallets",
           fill = "Segment") +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      ylim(0, NA)  +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    ggplotly(G_Wallets)
  })
  output$G_Percentage = renderPlotly(({
    day_information <- data %>%
      select(c("Date", "Seg",  "Wallets")) %>%
      filter(Date == as.Date(input$date, format = "%Y-%m-%d")) %>%
      mutate(all = Wallets[1]) %>%
      mutate(percentage = Wallets / all) %>%
      filter(Seg != "all") %>%
      mutate(all = "all") %>%
      select("Seg", "all", "percentage")
    day_information <- day_information %>%
      add_row(Seg = "other",
              all = "all",
              percentage = (1 - sum(day_information$percentage)))
    gg <- day_information  %>%
      ggplot() +
      geom_bar(aes(x = all, y = percentage, fill = Seg),
               stat = "identity",
               width = 0.5) +
      labs(title = "Percentage of Segments in All Wallets",
           y = "Percentage",
           fill = "Segment") +
      theme_classic() +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      ) +
      scale_y_continuous(labels = scales::percent)
    ggplotly(gg)
  }))
  
  
  
  #Page3
  
  
  output$value <- renderPrint({
    input$lambda
  })
  output$value <- renderPrint({
    input$daynumber
  })
  output$pathnumber <- renderPrint({
    input$pathnumber
  })
  output$Portfolio <- renderPrint({
    input$Portfolio
  })
  
  daily_return <- function(data, name) {
    data <- data %>%
      mutate(lag_1 = lag(Adj.Close)) %>%
      mutate(log_return = log(Adj.Close) - log(lag_1))
    return(data)
  }
  
  NFTZ <- daily_return(nftz)
  NFTZ$Date <- as.Date(NFTZ$Date, format = "%Y-%m-%d")
  NFTZ$log_return[1] = 0.01
  end = length(NFTZ$Date)
  
  NFTZ_Simulation <- reactive({
    N = input$pathnumber
    mu = mean(NFTZ$log_return)
    sigma = sd(NFTZ$log_return)
    S_0 = NFTZ$Adj.Close[end]
    dt = 1 / 252
    
    Simulation_M <- function(N, t, mu, sigma, S_0, dt) {
      S <- matrix(0, t, N)
      for (i in 1:N) {
        S[1, i] <- S_0
        for (j in 1:(t - 1)) {
          Z <- rnorm(1, 0, sqrt(dt))
          S[j + 1, i] <-
            S[j, i] * exp((mu - sigma ^ 2 / 2) * dt + sigma * Z)
          
        }
      }
      return(S)
    }
    S <- Simulation_M(N, input$daynumber, mu, sigma, S_0, dt)
    
    NFTZ_Simulation <- as.data.frame(S) %>%
      mutate(Date = seq(NFTZ$Date[end] + 1, NFTZ$Date[end] + input$daynumber, 1)) %>%
      pivot_longer(-Date, names_to = 'sim', values_to = 'price')
    return(NFTZ_Simulation)
  })
  
  E_return <- reactive({
    E_return <- NFTZ_Simulation() %>%
      group_by(Date) %>%
      summarise(Adj.Close = mean(price))
    return(E_return)
  })
  
  
  Prediction_Simulation <- reactive({
    Prediction_Simulation <- NFTZ_Simulation() %>%
      filter(Date == as.Date(NFTZ$Date[end] + input$daynumber, format = "%Y-%m-%d")) %>%
      mutate(return = round((price / NFTZ$Adj.Close[end] - 1), 3)) %>%
      arrange(return, descending = TRUE) %>%
      mutate(win = if_else(return >= 0, 1, 0)) %>%
      mutate(label = if_else(return >= 0,
                             "Positive",
                             "Negative"))
    return(Prediction_Simulation)
  })
  
  #Simulation
  
  output$G_Simulation <- renderPlotly({
    NFTZ_Simulation() %>%
      ggplot(aes(x = Date, y = price, color = sim)) +
      geom_line() +
      theme(
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      labs(title = paste(input$pathnumber, "Times Simulation Outcome"),
           y = "Price")
  })
  
  output$G_hist_Simulation = renderPlotly({
    G_hist_Simulation <- Prediction_Simulation() %>%
      ggplot(aes(x = return)) +
      geom_histogram(aes(y = ..density.., fill = label),
                     bins = 25,
                     colour = 1) +
      geom_density(size = 1) +
      labs(
        title = paste(
          "Distribution of Return in Prediction in the End of",
          input$daynumber,
          "Days"
        ) ,
        x = "Return",
        y = "Density",
        colour = "Gender"
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()
      )
    ggplotly(G_hist_Simulation)
  })
  
  output$G_Expected = renderPlotly({
    Graph_Simulation <- NFTZ %>%
      select(c("Date", "Adj.Close")) %>%
      rbind(E_return()) %>%
      mutate(color = if_else(Date >= NFTZ$Date[end], "prediction", "historical"))
    
    Graph_Simulation$Date = as.Date(Graph_Simulation$Date, format = "%Y-%m-%d")
    G_Expected <-
      Graph_Simulation %>%  ggplot(mapping = aes(
        x = Date,
        y = Adj.Close,
        color = color,
        group = 1
      )) +
      geom_path() +
      geom_vline(xintercept = NFTZ$Date[end], linetype = "dashed") +
      labs(title = "Price Trend", y = "Price") +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
    ggplotly(G_Expected)
  })
  
  
  #Some Conclusion(Done)
  output$win_0.01 = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <-
      mean(temp_df$return[round(input$pathnumber * 0.99)])
    return(paste(temp_data * 100, "%"))
  })
  output$win_0.05 = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <-
      mean(temp_df$return[round(input$pathnumber * 0.95)])
    return(paste(temp_data * 100, "%"))
  })
  output$P_win = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <- round(mean(temp_df$win), 4)
    return(paste(temp_data * 100, "%"))
  })
  output$lose_0.01 = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <-
      mean(temp_df$return[round(input$pathnumber * 0.01)])
    return(paste(temp_data * -100, "%"))
  })
  output$lose_0.05 = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <-
      mean(temp_df$return[round(input$pathnumber * 0.05)])
    return(paste(temp_data * -100, "%"))
  })
  output$Expected_return = renderText({
    temp_df <- Prediction_Simulation()
    temp_data <- round(mean(temp_df$return), 4)
    return(paste(temp_data * 100, "%"))
  })
  
  #Hedge
  
  
  Return_data <- reactive({
    if (input$Portfolio == "Long NFTZ& Short ETH") {
      temp_NFTZ <- daily_return(NFTZ) %>%
        mutate(NFTZ_return = 1 * log_return)
      temp_ETH <- daily_return(ETH) %>%
        mutate(ETH_return = -1 * log_return)
      Return_data <- temp_NFTZ  %>%
        select("Date", "NFTZ_return") %>%
        inner_join(temp_ETH %>%
                     select("Date", "ETH_return"),
                   by = c("Date" = "Date")) %>%
        na.omit()
    } else{
      temp_NFTZ <- daily_return(NFTZ) %>%
        mutate(NFTZ_return = -1 * log_return)
      temp_ETH <- daily_return(ETH) %>%
        mutate(ETH_return = 1 * log_return)
      Return_data <- temp_NFTZ  %>%
        select("Date", "NFTZ_return") %>%
        inner_join(temp_ETH %>%
                     select("Date", "ETH_return"),
                   by = c("Date" = "Date")) %>%
        na.omit()
    }
    return(Return_data)
  })
  portfolio_values <- reactive({
    mean_ret <- colMeans(Return_data() %>%
                           select(c(2, 3)))
    cov_mat <- cov(Return_data() %>%
                     select(c(2, 3)))
    r = 0.00
    num_port <- 1000
    all_wei <- matrix(nrow = num_port,
                      ncol = 2)
    port_returns <- vector('numeric', length = num_port)
    port_risk <- vector('numeric', length = num_port)
    sharpe_ratio <- vector('numeric', length = num_port)
    
    for (i in seq_along(port_returns)) {
      wei_start <- runif(2)
      wei_start <- wei_start / sum(wei_start)
      all_wei[i, ] <- wei_start
      port_ret <- (sum(wei_start * mean_ret) + 1) ^ 1 - 1
      port_returns[i] <- port_ret
      port_sd <- sqrt(t(wei_start) %*% (cov_mat  %*% wei_start))
      port_risk[i] <- port_sd
      sr <- port_ret / port_sd
      sharpe_ratio[i] <- sr
      
    }
    portfolio_values <- tibble(Return = port_returns,
                               Risk = port_risk,
                               SharpeRatio = sharpe_ratio)
    all_wei <- tk_tbl(all_wei)
    
    colnames(all_wei) <- colnames(Return_data() %>%
                                    select(c(2, 3)))
    portfolio_values <- tk_tbl(cbind(all_wei, portfolio_values))
    return(portfolio_values)
  })
  min_var <- reactive({
    temp <- portfolio_values()
    temp_out <- temp[which.min(temp$Risk), ]
  })
  max_sr <- reactive({
    temp <- portfolio_values()
    temp_out <- temp[which.max(temp$SharpeRatio), ]
  })
  output$G_efficient_frontier <- renderPlotly({
    G_efficient_frontier <- portfolio_values() %>%
      select("Risk", "Return", "SharpeRatio") %>%
      ggplot(mapping = aes(x = Risk, y = Return, color = SharpeRatio)) +
      geom_point() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = 'Daily Risk',
           y = 'Daily Returns',
           title = "Portfolio Optimization & Efficient Frontier") +
      geom_point(aes(x = Risk,
                     y = Return),
                 data = min_var(),
                 color = 'red') +
      geom_point(aes(x = Risk,
                     y = Return),
                 data = max_sr(),
                 color = 'red')
    
    
    ggplotly(G_efficient_frontier)
  })
  output$G_portfolio_composition <- renderPlotly({
    G_portfolio_composition <- max_sr() %>%
      rename("NFTZ" = "NFTZ_return",
             "ETH" = "ETH_return") %>%
      mutate(Seg = "Max Sharpe Ratio") %>%
      full_join(
        min_var() %>%
          mutate(Seg = "Min Variance") %>%
          rename("NFTZ" = "NFTZ_return",
                 "ETH" = "ETH_return")
      ) %>%
      pivot_longer(c(1, 2), names_to = "ETF", values_to = "weight") %>%
      ggplot() +
      geom_bar(aes(x = weight, y = Seg, fill = ETF), stat = "identity") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(
        title = "Portfolio Composition Under Two Strategies",
        x = "Weight",
        y = " Strategy",
        fill = "Asset"
      )
    ggplotly(G_portfolio_composition)
  })
  output$G_history_Performance <- renderPlotly({
    temp_max_sr <- max_sr()
    temp_min_var <- min_var()
    max_NFTZ_Wei =  temp_max_sr$NFTZ_return[1]
    max_ETH_Wei =  temp_max_sr$ETH_return[1]
    min_NFTZ_Wei =  temp_min_var$NFTZ_return[1]
    min_ETH_Wei =  temp_min_var$ETH_return[1]
    
    
    temp_data <-  Return_data() %>%
      mutate(Maximize_Shape_Ratio = NFTZ_return * max_NFTZ_Wei + ETH_return * max_ETH_Wei) %>%
      mutate(Minimize_Variance = NFTZ_return * min_NFTZ_Wei  + ETH_return * min_ETH_Wei)
    
    temp_data$Date <- as.Date(temp_data$Date, format = "%Y-%m-%d")
    G_history_Performance <- temp_data %>%
      select("Date", "Maximize_Shape_Ratio", "Minimize_Variance") %>%
      pivot_longer(c(2, 3), names_to = "Seg", values_to = "return") %>%
      ggplot() +
      geom_boxplot(aes(x = Seg, y = return, color = Seg)) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none'
      ) +
      labs(title = "Portfolio History Return Comparsion",
           x = " Portfolio",
           y = "Return")
    ggplotly(G_history_Performance)
  })
  
  output$NFTZ_max = renderText({
    temp <- max_sr()
    tempp <- paste(round(temp[1], 3) * 100, "%")
  })
  output$ETH_max = renderText({
    temp <- max_sr()
    tempp <- paste(round(temp[2], 3) * 100, "%")
  })
  output$MR_max = renderText({
    temp <- max_sr()
    tempp <- paste(round(temp[3], 3) * 100, "%")
  })
  output$Risk_max = renderText({
    temp <- max_sr()
    tempp <- paste(round(temp[4], 3) * 100, "%")
  })
  output$NFTZ_min = renderText({
    temp <- min_var()
    tempp <- paste(round(temp[1], 3) * 100, "%")
  })
  output$ETH_min = renderText({
    temp <- min_var()
    tempp <- paste(round(temp[2], 3) * 100, "%")
  })
  output$MR_min = renderText({
    temp <- min_var()
    tempp <- paste(round(temp[3], 3) * 100, "%")
  })
  output$Risk_min = renderText({
    temp <- min_var()
    tempp <- paste(round(temp[4], 3) * 100, "%")
  })
  
  
  #Page4(Done)
  output$NFT_Market = renderDataTable({
    return(datatable(data, rownames = FALSE))
  })
  output$NFT_Tweet = renderDataTable({
    return(datatable(nft_tweet, rownames = FALSE))
  })
  output$NFTZ = renderDataTable({
    return(datatable(nftz, rownames = FALSE))
  })
  output$ETH = renderDataTable({
    return(datatable(ETH, rownames = FALSE))
  })
  
  #after all codes
  removeUI(selector = '#Welcome')
}

# Run the application
shinyApp(ui = ui, server = server)
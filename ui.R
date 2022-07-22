library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(slickR)
library(wordcloud)
library(scales)
library(timetk)
library(ggplot2)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "NFT Analysis", titleWidth  = 550),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Welcome", tabName = "page1", icon = icon("home")),
      
      menuItem(
        "Bet on Market Information",
        tabName = "page2",
        icon = icon("line-chart")
      ),
      
      menuItem(
        "Bet on Strategies",
        tabName = "page3",
        icon = icon("user")
      ),
      
      menuItem("Database", tabName = "page4", icon = icon("database"))
    )
  ),
  
  dashboardBody(tabItems(
    #Page1
    tabItem(
      tabName = "page1",
      
      div(
        id = 'Welcome',
        h1('Welcome to the NFT world !',
           style = "color:darkblue" , align = "center") ,
        hr()
      ),
      tags$style(
        HTML(
          "
                .box.box-solid.box-primary>.box-header {
                background:	#0C4876
                }
                .box.box-solid.box-primary{
                }
                "
        )
      ),
      box(
        width = 20,
        title = "Introduction",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(
            7,
            h4(
              "A non-fungible token (NFT) is a financial security consisting of digital data stored in a blockchain, a form of distributed ledger.
                             The ownership of an NFT is recorded in the blockchain and can be transferred by the owner. Because NFTs are uniquely identifiable,
                             they differ from cryptocurrencies, which are fungible. With the increase in the popularity of digital currency, a new digital commodity has appeared in the public eye,
                             that is, NFTs that use digital currency for transactions. The classification of NFT includes art, game-related items, collectibles, etc."
            ),
            hr(),
            h4("Research Questions:"),
            h4(
              "1. What is the market trend of number of sales/traders/active wallets on each NFT segment?"
            ),
            h4("2. What is the predicted trend of NFT trading in the next few days?"),
            h4("3. Is NFT a good asset to hedge the risk of cryptocurrency?"),
            h4(
              "4. Under different strategy, what portfolio will generate highest Sharpe ratio and lowest variance?"
            ),
            h4("5. How significant is each hedging strategy?"),
          ),
          column(5,
                 plotOutput("WordCloud"),)
        ),
      ),
      hr(),
      box(
        width = 20,
        title = "Top 3 NFTs",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(
            7,
            h4("Everydays-The First 5000 Days"),
            h5(
              "The artwork, a compendium of the artist's over-13-year daily drawing project,
                             opened for bidding at just $100 before skyrocketing to $69 million, becoming the first-most expensive NFT works."
            ),
            h4("Pak's Clock"),
            h5(
              "The piece, which represents the number of days Julian Assange has been in prison, was part of the Censored NFT collection
                           sold to pay the Wikileaks founder's legal fees as he fights extradition to the U.S. from London. The winning bidder,
                           a 10,000-person collective called AssangeDAO, shelled out $52 million."
            ),
            h4("Human One"),
            h5(
              "The NFT of Human One is an ongoing journey that changes based on time of day and will continue to evolve over time.
                           Hidden in the video will be clues to unlock additional NFTs. These NFTs will be an edition of 2 with one going to the winner,
                           and the other going to the owner of it.
                           The latest highest sale price is $29.8 million."
            ),
          ),
          column(5,
                 slickROutput(
                   "slickr", width = "400px", height = "400px"
                 ),)
        ),
      ),
      hr(),
      box(
        width = 20,
        title = "Group 128",
        status = "primary",
        solidHeader = TRUE,
        h4("Team Member"),
        h5("Wenbo Han, Ruoyi Duan, Xinyang Li, and we are Carey MSF graduates."),
      ),
      
    ),
    
    #Page2
    tabItem(
      tabName = "page2",
      box(
        width = 20,
        height = 600,
        title = "Market Trend on Number of Sales",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          3,
          checkboxGroupInput(
            "show_vars",
            "Choose your variables :",
            c("all", "art", "collectible", "game", "metaverse", "utility"),
            selected = "all"
          ),
          
        ),
        
        column(9,
               plotlyOutput("G_Market_Trend"))),
      ),
      
      hr(),
      box(
        width = 20,
        height = 1000,
        title = "Market Trend on Traders",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          3,
          sliderInput(
            "year",
            "Year:",
            min = as.Date("2017-11-09", "%Y-%m-%d"),
            max = as.Date("2022-06-17", "%Y-%m-%d"),
            value = as.Date("2022-01-01", timeFormat = "%Y-%m-%d"),
            step = 21,
            animate = animationOptions(interval = 1000, loop = FALSE)
          ),
        ),
        column(
          9,
          plotlyOutput("G_Buyer_Seller", height = 800)
        )),
      ),
      hr(),
      box(
        width = 20,
        height = 1000,
        title = "Market Trend on Active Wallets",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          4,
          dateInput(
            "date",
            "Please choose a date you are interested:",
            min = as.Date("2017-11-09", "%Y-%m-%d"),
            max = as.Date("2022-06-17", "%Y-%m-%d"),
            value = as.Date("2022-01-01", timeFormat = "%Y-%m-%d"),
          ),
          plotlyOutput("G_Percentage", height = 600, width = 400)
          
        ),
        column(
          8,
          plotlyOutput("G_Wallets", height = 800, width = 1000)
        )),
      ),
      
    ),
    
    #Page3
    tabItem(
      tabName = "page3",
      box(
        width = 20,
        height = 1400,
        title = "Simulation Strategy",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(
            6,
            numericInput(
              "daynumber",
              label = h3("Choose the number of days to simulate: "),
              min = 2,
              value = 10,
              width = 500
            ),
            numericInput(
              "pathnumber",
              label = h3("Choose the number of paths to simulate: "),
              value = 100,
              width = 500,
              min = 10,
              max  = 500,
              step = 10
            ),
          ),
          column(
            6,
            h4("Instructional video"),
            HTML(
              '<iframe width="400" height="300"
                      src="https://www.youtube.com/embed/--6F3rt1qY8" frameborder="0"
                      allowfullscreen></iframe>'
            )
            
          ),
          
        ),
        
        
        fluidRow(
          column(6,
                 column(
                   6,
                   box(
                     width = 35,
                     title = "Expected Daily Return:",
                     status = "primary",
                     solidHeader = TRUE,
                     textOutput("Expected_return")
                   )
                 ),
                 column(
                   6,
                   box(
                     width = 20,
                     title = "Probability of Win:",
                     status = "primary",
                     solidHeader = TRUE,
                     textOutput("P_win")
                   )
                 ),),
          column(
            6,
            column(
              3,
              box(
                width = 15,
                title = "5% to Win:",
                status = "primary",
                solidHeader = TRUE,
                textOutput("win_0.05")
              )
            ),
            column(
              3,
              box(
                width = 15,
                title = "1% to Win:",
                status = "primary",
                solidHeader = TRUE,
                textOutput("win_0.01")
              )
            ),
            column(
              3,
              box(
                width = 15,
                title = "5% to Lose:",
                status = "primary",
                solidHeader = TRUE,
                textOutput("lose_0.05")
              )
            ),
            column(
              3,
              box(
                width = 15,
                title = "1% to Lose:",
                status = "primary",
                solidHeader = TRUE,
                textOutput("lose_0.01")
              )
            ),
            
          ),
          
        ),
        fluidRow(
          column(6,
                 
                 plotlyOutput("G_hist_Simulation", height = 800)),
          column(
            6,
            
            plotlyOutput("G_Simulation", height = 400, width = 600),
            hr(),
            
            plotlyOutput("G_Expected", height = 400, width = 600),
            
          )
          
        ),
      ),
      
      hr(),
      
      box(
        width = 20,
        height = 1000,
        title = "Hedge Strategy",
        status = "primary",
        solidHeader = TRUE,
        selectInput(
          "Portfolio",
          h3("Choose your portfolio:"),
          c("Long NFTZ& Short ETH",
            "Short NFTZ& Long ETH"),
          selected = "Long NFTZ& Short ETH"
        ),
        h3("Portfolio Information Under Maximization Sharpe Ratio"),
        fluidRow(
          column(
            3,
            box(
              width = 20,
              title = "NFTZ Weight",
              status = "primary",
              solidHeader = TRUE,
              textOutput("NFTZ_max")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "ETH weight",
              status = "primary",
              solidHeader = TRUE,
              textOutput("ETH_max")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "Daily Return",
              status = "primary",
              solidHeader = TRUE,
              textOutput("MR_max")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "Risk",
              status = "primary",
              solidHeader = TRUE,
              textOutput("Risk_max")
            )
          ),
        ),
        
        h3("Portfolio Information Under Minimize Variance"),
        fluidRow(
          column(
            3,
            box(
              width = 20,
              title = "NFTZ Weight",
              status = "primary",
              solidHeader = TRUE,
              textOutput("NFTZ_min")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "ETH weight",
              status = "primary",
              solidHeader = TRUE,
              textOutput("ETH_min")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "Daily Return",
              status = "primary",
              solidHeader = TRUE,
              textOutput("MR_min")
            )
          ),
          column(
            3,
            box(
              width = 20,
              title = "Risk",
              status = "primary",
              solidHeader = TRUE,
              textOutput("Risk_min")
            )
          ),
        ),
        
        fluidRow(
          column(4,
                 plotlyOutput("G_efficient_frontier")),
          column(4,
                 plotlyOutput("G_portfolio_composition")),
          column(4,
                 plotlyOutput("G_history_Performance")),
          
        ),
        
      ),
    ),
    
    ##############################################################################3
    
    
    #Page4
    tabItem(
      tabName = "page4",
      tags$strong(h1("Data Source")),
      h4(
        "  Our website is established on the below dataset. If you have further interest, please click the 'click here' button in each dataset to visit the data source"
      ),
      
      tags$strong(h3("NFT Market")),
      h5(
        "This dataset is collected from Nonfungible webstie. We download all datasets and merge them into one dataset."
      ),
      tags$a(href = "https://nonfungible.com/market-tracker", "Click here!"),
      dataTableOutput("NFT_Market"),
      
      tags$strong(h3("NFT Tweets")),
      h5(
        "This dataset is downloaded from Kaggle. We build our nnword dataset based on this dataset to save running time of our website."
      ),
      tags$a(href = "https://www.kaggle.com/datasets/mathurinache/nft-tweets", "Click here!"),
      dataTableOutput("NFT_Tweet"),
      
      tags$strong(h3("NFTZ(ETF of NFT)")),
      h5(
        "  This dataset is downloaded from Yahoo Finance. The name of NFTZ is 'Defiance Digital Revolution ETF'. This ETF invest in the digital economy with NFTs, Blockchain and the NFT market place."
      ),
      tags$a(href = "https://finance.yahoo.com/quote/NFTZ?p=NFTZ&.tsrc=fin-srch", "Click here!"),
      dataTableOutput("NFTZ"),
      
      tags$strong(h3("ETH(Ethereum-USD)")),
      h5("This dataset is downloaded from Yahoo Finance."),
      tags$a(href = "https://finance.yahoo.com/quote/ETH-USD?p=ETH-USD&.tsrc=fin-srch", "Click here!"),
      dataTableOutput("ETH"),
      tags$br(),
    )
    
  ))
)

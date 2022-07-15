library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(slickR)
library(wordcloud)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NFT Analysis", titleWidth  = 550),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Welcome", tabName = "page1", icon = icon("home")),
      
      menuItem("Bet on Market Trend", tabName = "page2", icon = icon("line-chart")),
      
      menuItem("Bet on Hedge", tabName = "page3", icon = icon("user")),
      
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
      
      box(width = 20, title = "Introduction", status = "primary", solidHeader = TRUE,
          fluidRow(
            column(7,
                   h4("TEXT")
            ),
            column(5,
                   slickROutput("slickr", width = "400px", height = "400px"),
            )
          ),
      ),
      hr(),
      box(width = 20, title = "NFT on Twitter", status = "primary", solidHeader = TRUE,
          fluidRow(
            column(7,
                   h4("TEXT")
            ),
            column(5,
                   plotOutput("WordCloud"),
            )
          ),
      ),
      hr(),
      box(width = 20, title = "Team Member", status = "primary", solidHeader = TRUE,
          fluidRow(
            column(3,
                   h4("Photo"),     
            ),
            column(6,
                   h4("Name"),
                   hr(), 
                   h5("Introduce yourself"),
                   hr(), 
            )
            
          ),
          fluidRow(
            column(3,
                   h4("Photo"),     
            ),
            column(6,
                   h4("Name"),
                   hr(), 
                   h5("Introduce yourself"),
                   hr(), 
            )
            
          ),
          fluidRow(
            column(3,
                   h4("Photo"),     
            ),
            column(6,
                   h4("Name"),
                   hr(), 
                   h5("Introduce yourself"),
                   hr(), 
            )
            
          ),
      ),
      
    ),
    
    #Page2
    tabItem(
      tabName = "page2",
      
      h2("Market Trend on Number of Sales"),
      
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        top = 85,
        right = 40,
        width = 200,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        tags$i(h6("Data Source:")),
        h6("Test Panel"),
        
      ),
      fluidRow(
        column(
          3,
          checkboxGroupInput(
            "show_vars",
            "Choose your variables :",
            c("all", "art", "collectible", "game", "metaverse", "utility")
          ),
          
          tags$h5("Begin"),
          actionButton("action", "Action button", class = "btn-primary"),
          
          tags$h5("Default actionButton:"),
          actionButton("action2", "Search All"),
        ),
        
        column(9,
               img(width = "700px", 
                   src = "markettrend.png"))
      ),
      
      
      hr(),
      h2("Average USD Sales"),
      fluidRow(column(
        3,
        sliderInput(
          "year",
          "Year:",
          min = as.Date("2017-11-09","%Y-%m-%d"),
          max = as.Date("2022-06-17","%Y-%m-%d"),
          value=as.Date("2019-12-31",timeFormat="%Y-%m-%d"),
          step = 1,
          animate = animationOptions(interval = 1000, loop = FALSE)
        ),
      ),
      column(9,
             img(
               width = "700px", 
               src = "averagesales.png"
             )
      )
      ),
      
      
    ),
    
    
    #Page3       
    tabItem(tabName = "page3",
            numericInput("datenumber", label = h3("Choose the number of date"), value = 1),
            
            tags$style(HTML("
                .box.box-solid.box-primary>.box-header {}
                .box.box-solid.box-primary{
                background:	#FFFFFF
                }
                ")),
            fluidRow(
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
              column(2,
                     box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                         "Box content"
                     )
              ),
            ),
            fluidRow(
              column(6,
                     
                     img(
                       width = "600px", 
                       src = "distribution.png"
                       ,)
                     
              ),
              column(6,
                     
                     img(
                       width = "600px", 
                       src = "pricetrend.png"
                       
                     ),
                     hr(),
                     
                     img(
                       width = "600px", 
                       src = "simulation.png"
                       
                     ),
                     
              ),
            ),
            hr(),
            
            fluidRow(
              column(3,
                     numericInput("lambda", label = h3("Please input your lambda in (0.1-1.0):"), 
                                  value = 1,step = 0.1, min = 0.1, max = 1.0),
              ),
              column(9,
                     column(4,
                            box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                                "Box content"
                            )
                     ),
                     column(4,
                            box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                                "Box content"
                            )
                     ),
                     column(4,
                            box(width = 20, title = "test", status = "primary", solidHeader = TRUE,
                                "Box content"
                            )
                     ),
              )
            ),
            img(
              width = "700px", 
              src = "rm.png"
            )
            
    ),
    
    
    
    
    
    #Page4
    tabItem(
      tabName = "page4",
      tags$strong(h1("Data Source")),
      h4(
        "  Our website is established on the below dataset. If you have further interest, please click the 'click here' button in each dataset to visit the data source"
      ),
      
      tags$strong(h3("NFT Market")),
      h5("This dataset is collected from Nonfungible webstie. We download all datasets and merge them into one dataset."),
      tags$a(href="https://nonfungible.com/market-tracker", "Click here!"),
      dataTableOutput("NFT_Market"),
      
      tags$strong(h3("NFT Tweets")),
      h5("This dataset is downloaded from Kaggle. We build our nnword dataset based on this dataset to save running time of our website."),
      tags$a(href="https://www.kaggle.com/datasets/mathurinache/nft-tweets", "Click here!"),
      dataTableOutput("NFT_Tweet"),
      
      tags$strong(h3("NFTZ(ETF of NFT)")),
      h5("  This dataset is downloaded from Yahoo Finance. The name of NFTZ is 'Defiance Digital Revolution ETF'. This ETF invest in the digital economy with NFTs, Blockchain and the NFT market place."),
      tags$a(href="https://finance.yahoo.com/quote/NFTZ?p=NFTZ&.tsrc=fin-srch", "Click here!"),
      dataTableOutput("NFTZ"),
      
      tags$strong(h3("ETH(Ethereum-USD)")),
      h5("This dataset is downloaded from Yahoo Finance."),
      tags$a(href="https://finance.yahoo.com/quote/ETH-USD?p=ETH-USD&.tsrc=fin-srch", "Click here!"),
      dataTableOutput("ETH"),
      tags$br(),
    )
    
  ))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Load_data
  nnwords <- read.csv("nnwords.csv")
  data = read.csv("data.csv")
  eth <- read.csv("eth.csv")
  nftz <- read.csv("nftz.csv")
  nft_tweet <- read.csv("nft_tweets.csv")
  
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
  output$value <- renderPrint({ input$show_vars})
  output$value <- renderPrint({ input$action })
  output$value <- renderPrint({ input$action2 })
  output$value <- renderPrint({ input$year })
  
  #Page3
  output$value <- renderPrint({ input$lambda })
  output$value <- renderPrint({ input$datenumber })
  
  
  #Page4
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
    return(datatable(eth, rownames = FALSE))
  })
  
  #after all codes
  removeUI( selector = '#Welcome' )
}

# Run the application
shinyApp(ui = ui, server = server)

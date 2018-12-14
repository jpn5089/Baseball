library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(DT)
# https://support.rstudio.com/hc/en-us/articles/229848967-Why-does-my-app-work-locally-but-not-on-shinyapps-io-

# library(rsconnect)
# rsconnect::deployApp('C://Users/johnp/Documents/GitHub/Baseball/Draught_Stats')

dates <- data.frame(dates = seq(ymd("2018-03-29"), ymd("2018-09-16"), by = 1)) %>% 
  filter(!row_number() %in% c(37,44,45,47,48,56,58,64,71,75,77,78,79,81,84,85,86,88,89,91,92,95,96,99,103,105,
                              107,108,111,112,113,114,119,120,123,128,130,133,134,136,139,141,142,144,
                              147,148,150:157,161,163,165,166,167,169,170)) %>% 
  pull()

stats <- readRDS(paste0("stats_", last(dates), ".rds"))
standings <- readRDS(paste0("standings_", last(dates), ".rds"))

ui <- navbarPage(
  "Draught 2018",
  tabPanel("Overall Standings",
           mainPanel(width = 10,
                     plotlyOutput("standings_plot")
                     
           )
  ),
  tabPanel("Stats",
           
           fluidPage(
             fluidRow(
               column(3,
                 dateRangeInput("daterange", "Zoom In:",
                                start  = "2018-03-29",
                                end    = max(dates),
                                min    = "2018-03-29",
                                max    = max(dates))),
               column(2,
                 selectInput('stat', 'Choose Stat', choices = c("BA", "OBP", "HR", "RBI", "R", "SB", "ERA",
                                                                "WHIP", "K", "QS", "HD", "S"), selected = "BA")),
               mainPanel(width = 12,
                         plotlyOutput("stats_plot")
                 
               )
             )
           )
          
  ),
  tabPanel("Weighted Stats",
           
           fluidPage(
             fluidRow(
               column(3,
                      dateRangeInput("daterange2", "Zoom In:",
                                     start  = "2018-03-29",
                                     end    = max(dates),
                                     min    = "2018-03-29",
                                     max    = max(dates))),
           
           mainPanel(width = 12,
             splitLayout(
               cellWidths = c("5%", "40%", "10%", "40%", "5%"),
               br(),
               DT::dataTableOutput("weighted_pts"),
               br(),
               DT::dataTableOutput("weighted_rankings"),
               br()))
             )
           )
  )
)

server <- function(input, output) {
  
   output$stats_plot <- renderPlotly({
     
     pdata <- stats %>% 
       filter(date >= input$daterange[1] & date <= input$daterange[2]) %>% 
       filter(stat == input$stat)
     
     title = input$stat
     
     
     
     p <- ggplot(pdata, aes(date, value, color = Team, text = Team)) +
       geom_line() +
       labs(title = title) +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())
     
     ggplotly(p, width = 1250, height = 625, tooltip = c("text", "y"))
   })
   
   output$standings_plot <- renderPlotly({
     
     pdata <- standings 
     
     p <- ggplot(pdata, aes(date, Total, color = Team, text = Team)) +
       geom_line() +
       labs(title = "Total Pts") +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())
     
     ggplotly(p, width = 1250, height = 700, tooltip = c("text", "y"))
   })
   
   output$weighted_pts <- renderDataTable({
     
     weighted_pts <- standings %>%
       filter(date >= input$daterange2[1] & date <= input$daterange2[2]) %>% 
       group_by(Team) %>%
       summarise(avg_pts = round(mean(Total), 1)) %>% 
       ungroup() %>% 
       arrange(desc(avg_pts))
     
     datatable(weighted_pts, options = list(pageLength = 14, bLengthChange = F))
   }) 
   
   output$weighted_rankings <- renderDataTable({
     
     weighted_rank <- standings %>%
       filter(date >= input$daterange2[1] & date <= input$daterange2[2]) %>% 
       mutate(Rank = as.integer(as.character(Rank))) %>% 
       group_by(Team) %>%
       summarise(avg_rank = round(mean(Rank), 1)) %>% 
       ungroup() %>% 
       arrange(avg_rank)
     
     datatable(weighted_rank, options = list(pageLength = 14, bLengthChange = F))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


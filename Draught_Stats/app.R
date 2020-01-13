library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(DT)
# https://support.rstudio.com/hc/en-us/articles/229848967-Why-does-my-app-work-locally-but-not-on-shinyapps-io-

# library(rsconnect)
# rsconnect::deployApp('C://Users/johnp/Documents/GitHub/Baseball/Draught_Stats')

dates_2019 <- data.frame(dates = seq(ymd("2019-03-27"), ymd("2019-09-08"), by = 1)) %>% 
  filter(!row_number() %in% c(22,28,30,37,39,44,46,47,51,54,55,57,63,65,66,68:72,74:84,86,88:94,96,98,
                              103:113,115:118,121:127,129:140,142,144:151,153,155:156,160:161,163:164)) %>%
  pull()

stats_2019 <- readRDS(paste0("stats_", last(dates_2019), ".rds"))
standings_2019 <- readRDS(paste0("standings_", last(dates_2019), ".rds"))

dates_2018 <- data.frame(dates = seq(ymd("2018-03-29"), ymd("2018-09-16"), by = 1)) %>% 
  filter(!row_number() %in% c(37,44,45,47,48,56,58,64,71,75,77,78,79,81,84,85,86,88,89,91,92,95,96,99,103,105,
                              107,108,111,112,113,114,119,120,123,128,130,133,134,136,139,141,142,144,
                              147,148,150:157,161,163,165,166,167,169,170)) %>% 
  pull()

stats_2018 <- readRDS(paste0("stats_", last(dates_2018), ".rds"))
standings_2018 <- readRDS(paste0("standings_", last(dates_2018), ".rds"))

ui <- navbarPage(
  "Draught",
  tabPanel("2019",
           textOutput("update"),
           br(),
           tabsetPanel(
             tabPanel("Overall Standings",
                      mainPanel(width = 10,
                                plotlyOutput("standings_plot_2019")
                      )
             ),
             tabPanel("Stats",
                      
                      fluidPage(
                        fluidRow(
                          column(3,
                                 dateRangeInput("daterange_2019", "Zoom In:",
                                                start  = "2019-03-28",
                                                end    = max(dates_2019),
                                                min    = "2019-03-27",
                                                max    = max(dates_2019))),
                          column(2,
                                 selectInput('stat_2019', 'Choose Stat', choices = c("BA", "OBP", "HR", "RBI", "R", "SB", "ERA",
                                                                                "WHIP", "K", "QS", "HD", "S"), selected = "BA")),
                          mainPanel(width = 12,
                                    plotlyOutput("stats_plot_2019")
                                    
                          )
                        )
                      )
                      
             ),
             tabPanel("Weighted Stats",
                      
                      fluidPage(
                        fluidRow(
                          column(3,
                                 dateRangeInput("daterange_2019_2", "Zoom In:",
                                                start  = "2019-03-28",
                                                end    = max(dates_2019),
                                                min    = "2019-03-27",
                                                max    = max(dates_2019))),
                          
                          mainPanel(width = 12,
                                    splitLayout(
                                      cellWidths = c("5%", "40%", "10%", "40%", "5%"),
                                      br(),
                                      DT::dataTableOutput("weighted_pts_2019"),
                                      br(),
                                      DT::dataTableOutput("weighted_rankings_2019"),
                                      br()))
                        )
                      )
             )
           )
  ),
  tabPanel("2018",
           br(),
           tabsetPanel(
             tabPanel("Overall Standings",
                      mainPanel(width = 10,
                                plotlyOutput("standings_plot_2018")
                      )
             ),
             tabPanel("Stats",
                      
                      fluidPage(
                        fluidRow(
                          column(3,
                                 dateRangeInput("daterange", "Zoom In:",
                                                start  = "2018-03-29",
                                                end    = max(dates_2018),
                                                min    = "2018-03-29",
                                                max    = max(dates_2018))),
                          column(2,
                                 selectInput('stat', 'Choose Stat', choices = c("BA", "OBP", "HR", "RBI", "R", "SB", "ERA",
                                                                                "WHIP", "K", "QS", "HD", "S"), selected = "BA")),
                          mainPanel(width = 12,
                                    plotlyOutput("stats_plot_2018")
                                    
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
                                                end    = max(dates_2018),
                                                min    = "2018-03-29",
                                                max    = max(dates_2018))),
                          
                          mainPanel(width = 12,
                                    splitLayout(
                                      cellWidths = c("5%", "40%", "10%", "40%", "5%"),
                                      br(),
                                      DT::dataTableOutput("weighted_pts_2018"),
                                      br(),
                                      DT::dataTableOutput("weighted_rankings_2018"),
                                      br()))
                        )
                      )
             )
           )
  )
)


server <- function(input, output) {
  
  output$update <- renderText({
    
    x <- paste0("Last Updated: ", last(dates_2019))
    
  })
  
   output$stats_plot_2018 <- renderPlotly({
     
     pdata <- stats_2018 %>% 
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
   
   output$standings_plot_2018 <- renderPlotly({
     
     pdata <- standings_2018 
     
     p <- ggplot(pdata, aes(date, Total, color = Team, text = Team)) +
       geom_line() +
       labs(title = "Total Pts") +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())
     
     ggplotly(p, width = 1250, height = 700, tooltip = c("text", "y"))
   })
   
   output$weighted_pts_2018 <- renderDataTable({
     
     weighted_pts <- standings_2018 %>%
       filter(date >= input$daterange2[1] & date <= input$daterange2[2]) %>% 
       group_by(Team) %>%
       summarise(avg_pts = round(mean(Total), 1)) %>% 
       ungroup() %>% 
       arrange(desc(avg_pts))
     
     datatable(weighted_pts, options = list(pageLength = 14, bLengthChange = F))
   }) 
   
   output$weighted_rankings_2018 <- renderDataTable({
     
     weighted_rank <- standings_2018 %>%
       filter(date >= input$daterange2[1] & date <= input$daterange2[2]) %>% 
       mutate(Rank = as.integer(as.character(Rank))) %>% 
       group_by(Team) %>%
       summarise(avg_rank = round(mean(Rank), 1)) %>% 
       ungroup() %>% 
       arrange(avg_rank)
     
     datatable(weighted_rank, options = list(pageLength = 14, bLengthChange = F))
   })
   
   output$stats_plot_2019 <- renderPlotly({
     
     pdata <- stats_2019 %>% 
       filter(date >= input$daterange_2019[1] & date <= input$daterange_2019[2]) %>% 
       filter(stat == input$stat_2019)
     
     title = input$stat_2019
     
     
     
     p <- ggplot(pdata, aes(date, value, color = Team, text = Team)) +
       geom_line() +
       labs(title = title) +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())
     
     ggplotly(p, width = 1250, height = 625, tooltip = c("text", "y"))
   })
   
   output$standings_plot_2019 <- renderPlotly({
     
     pdata <- standings_2019
     
     p <- ggplot(pdata, aes(date, Total, color = Team, text = Team)) +
       geom_line() +
       labs(title = "Total Pts") +
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank())
     
     ggplotly(p, width = 1250, height = 700, tooltip = c("text", "y"))
   })
   
   output$weighted_pts_2019 <- renderDataTable({
     
     weighted_pts <- standings_2019 %>%
       filter(date >= input$daterange_2019_2[1] & date <= input$daterange_2019_2[2]) %>% 
       group_by(Team) %>%
       summarise(avg_pts = round(mean(Total), 1)) %>% 
       ungroup() %>% 
       arrange(desc(avg_pts))
     
     datatable(weighted_pts, options = list(pageLength = 14, bLengthChange = F))
   }) 
   
   output$weighted_rankings_2019 <- renderDataTable({
     
     weighted_rank <- standings_2019 %>%
       filter(date >= input$daterange_2019_2[1] & date <= input$daterange_2019_2[2]) %>% 
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


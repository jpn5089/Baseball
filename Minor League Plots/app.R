library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis)
library(shinyjs)

# devtools::install_github("AliciaSchep/gglabeller")
# https://support.rstudio.com/hc/en-us/articles/229848967-Why-does-my-app-work-locally-but-not-on-shinyapps-io-

# library(rsconnect)
# rsconnect::deployApp('C://Users/johnp/Documents/GitHub/Baseball/Minor League Plots')

get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

AA_hitting <- readRDS("AA_hitting_all.rds")

high_A_hitting <- readRDS("highA_hitting_all.rds")

mid_A_hitting <- readRDS("midA_hitting_all.rds")

all_hitting <- Reduce(rbind, list(AA_hitting, high_A_hitting, mid_A_hitting))

AA_pitching <- readRDS("AA_pitching.rds")

high_A_pitching <- readRDS("highA_pitching.rds")

mid_A_pitching <- readRDS("midA_pitching.rds")

all_pitching <- Reduce(rbind, list(AA_pitching, high_A_pitching, mid_A_pitching))

ui <- navbarPage(
  "Minor League Stats",
  tabPanel("Batters",
           fluidPage(
             useShinyjs(),
             fluidRow(
               column(1, 
                      selectInput("level_batter", "Level", choices = c("A", "A+", "AA"), selected = "AA", multiple = F)
                      ),
                      # c("R", "A-", "A", "A+", "AA", "AAA")
               column(2,
                      selectInput("x_batter", "X-axis Var", choices = names(all_hitting)[c(2,4:22, 24:25)], selected = "BABIP",
                                  multiple = F)
               ),
               column(2,
                      selectInput("y_batter", "Y-axis Var", choices = names(all_hitting)[c(2,4:22, 24:25)], selected = "OBP",
                                  multiple = F)
               ),
               column(1,
                      numericInput('x_label_batter', 'X-axis Filter', value = NULL, min = -200, max = 1000)
               ),
               column(2,
                      radioButtons('batter_x_direction', 'X-axis Label Direction', 
                                   choices = c('Greater Than', 'Less Than'), selected = "Less Than")
               ),
               column(1,
                      numericInput('y_label_batter', 'Y-axis Filter', value = NULL, min = -200, max = 1000)
               ),
               column(2,
                      radioButtons('batter_y_direction', 'Y-axis Label Direction', 
                                   choices = c('Greater Than', 'Less Than'), selected = "Greater Than")
               ),
               column(1,
                      actionButton("refresh_batter", "Refresh")
                      ),
               mainPanel(width = 10,
                         plotOutput("batter_plot")
               )
             )
           )
  ),
  tabPanel("Pitchers",
           fluidPage(
             useShinyjs(),
             fluidRow(
               column(1, 
                      selectInput("level_pitcher", "Level", choices = c("A", "A+", "AA"), selected = "AA", multiple = F)
               ),
               # c("R", "A-", "A", "A+", "AA", "AAA")
               column(2,
                      selectInput("x_pitcher", "X-axis Var", choices = names(all_pitching)[c(2:26)], selected = "Age",
                                  multiple = F)
               ),
               column(2,
                      selectInput("y_pitcher", "Y-axis Var", choices = names(all_pitching)[c(2:26)], selected = "xFIP",
                                  multiple = F)
               ),
               column(1,
                      numericInput('x_label_pitcher', 'X-axis Filter', value = NULL, min = -200, max = 1000)
               ),
               column(2,
                      radioButtons('pitcher_x_direction', 'X-axis Label Direction', 
                                   choices = c('Greater Than', 'Less Than'), selected = "Less Than")
               ),
               column(1,
                      numericInput('y_label_pitcher', 'Y-axis Filter', value = NULL, min = -200, max = 1000)
               ),
               column(2,
                      radioButtons('pitcher_y_direction', 'Y-axis Label Direction', 
                                   choices = c('Greater Than', 'Less Than'), selected = "Greater Than")
               ),
               column(1,
                      actionButton("refresh_pitcher", "Refresh")
               ),
               mainPanel(width = 10,
                         plotOutput("pitcher_plot")
               )
             )
           )
  )
)

server <- function(input, output) {
  
  observeEvent(input$x_batter, {
    reset("x_label_batter")
  })
  
  observeEvent(input$y_batter, {
    reset("y_label_batter")
  })
  
  observeEvent(input$x_pitcher, {
    reset("x_label_pitcher")
  })
  
  observeEvent(input$y_pitcher, {
    reset("y_label_pitcher")
  })
  
  reac_batter <- reactiveValues(refresh_batter = TRUE, 
                         x_batter = isolate(input$x_batter),
                         y_batter = isolate(input$y_batter),
                         batter_x_direction = isolate(input$batter_x_direction),
                         x_label_batter = isolate(input$x_label_batter),
                         batter_y_direction = isolate(input$batter_y_direction),
                         y_label_batter = isolate(input$y_label_batter),
                         level_batter = isolate(input$level_batter))
  
  
  
  observe({
    input$x_batter
    input$y_batter
    input$batter_x_direction
    input$x_label_batter
    input$batter_y_direction
    input$y_label_batter
    input$level_batter
    reac_batter$refresh_batter <- FALSE
    
  })
  
  observe({

    input$x_batter
    input$y_batter
    input$batter_x_direction
    input$x_label_batter
    input$batter_y_direction
    input$y_label_batter
    input$level_batter
    input$refresh_batter
    
    isolate(cat(input$x_batter, input$y_batter, input$batter_x_direction, input$x_label_batter, input$batter_y_direction, 
                input$y_label_batter, input$level_batter, "\n"))
    
    if (isolate(reac_batter$refresh_batter)) {
      
      reac_batter$x_batter <- input$x_batter
      reac_batter$y_batter <- input$y_batter
      reac_batter$batter_x_direction <- input$batter_x_direction
      reac_batter$x_label_batter <- input$x_label_batter
      reac_batter$batter_y_direction <- input$batter_y_direction
      reac_batter$y_label_batter <- input$y_label_batter
      reac_batter$level_batter <- input$level_batter
      
    } else {
      
      isolate(reac_batter$refresh_batter <- TRUE)
      
    }
    
  })
  
  reac_pitcher <- reactiveValues(refresh_pitcher = TRUE, 
                                x_pitcher = isolate(input$x_pitcher),
                                y_pitcher = isolate(input$y_pitcher),
                                pitcher_x_direction = isolate(input$pitcher_x_direction),
                                x_label_pitcher = isolate(input$x_label_pitcher),
                                pitcher_y_direction = isolate(input$pitcher_y_direction),
                                y_label_pitcher = isolate(input$y_label_pitcher),
                                level_pitcher = isolate(input$level_pitcher))
  
  
  
  observe({
    input$x_pitcher
    input$y_pitcher
    input$pitcher_x_direction
    input$x_label_pitcher
    input$pitcher_y_direction
    input$y_label_pitcher
    input$level_pitcher
    reac_pitcher$refresh_pitcher <- FALSE
    
  })
  
  observe({
    
    input$x_pitcher
    input$y_pitcher
    input$pitcher_x_direction
    input$x_label_pitcher
    input$pitcher_y_direction
    input$y_label_pitcher
    input$level_pitcher
    input$refresh_pitcher
    
    isolate(cat(input$x_pitcher, input$y_pitcher, input$pitcher_x_direction, input$x_label_pitcher, input$pitcher_y_direction, 
                input$y_label_pitcher, input$level_pitcher, "\n"))
    
    if (isolate(reac_pitcher$refresh_pitcher)) {
      
      reac_pitcher$x_pitcher <- input$x_pitcher
      reac_pitcher$y_pitcher <- input$y_pitcher
      reac_pitcher$pitcher_x_direction <- input$pitcher_x_direction
      reac_pitcher$x_label_pitcher <- input$x_label_pitcher
      reac_pitcher$pitcher_y_direction <- input$pitcher_y_direction
      reac_pitcher$y_label_pitcher <- input$y_label_pitcher
      reac_pitcher$level_pitcher <- input$level_pitcher
      
    } else {
      
      isolate(reac_pitcher$refresh_pitcher <- TRUE)
      
    }
    
  })
  
  output$batter_plot <- renderPlot({

    data <- all_hitting %>% 
      filter(level == reac_batter$level_batter) %>% 
      select(name, x = reac_batter$x_batter, y = reac_batter$y_batter) %>%
      na.omit() %>% 
      mutate(x = as.numeric(x),
             y = as.numeric(y),
             density = get_density(x, y))
    
    if (reac_batter$batter_x_direction == "Less Than" & reac_batter$batter_y_direction == "Less Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_batter$y_batter, x = reac_batter$x_batter,
             caption = "Last 10 seasons of players with at least 200 PA. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y < reac_batter$y_label_batter& data$x < reac_batter$x_label_batter,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_batter$batter_x_direction == "Less Than" & reac_batter$batter_y_direction == "Greater Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_batter$y_batter, x = reac_batter$x_batter,
             caption = "Last 10 seasons of players with at least 200 PA. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y > reac_batter$y_label_batter& data$x < reac_batter$x_label_batter,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_batter$batter_x_direction == "Greater Than" & reac_batter$batter_y_direction == "Greater Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_batter$y_batter, x = reac_batter$x_batter,
             caption = "Last 10 seasons of players with at least 200 PA. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y > reac_batter$y_label_batter& data$x > reac_batter$x_label_batter,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_batter$batter_x_direction == "Greater Than" & reac_batter$batter_y_direction == "Less Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_batter$y_batter, x = reac_batter$x_batter,
             caption = "Last 10 seasons of players with at least 200 PA. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y < reac_batter$y_label_batter& data$x > reac_batter$x_label_batter,], size = 2.5) +
        theme(legend.position="none") #+ geom_abline(slope = 1)
      
    }
  },
  width = 1250, height = 700)

output$pitcher_plot <- renderPlot({
  
    data <- all_pitching %>% 
      filter(level == reac_pitcher$level_pitcher) %>% 
      select(name, x = reac_pitcher$x_pitcher, y = reac_pitcher$y_pitcher) %>%
      mutate(x = as.numeric(x),
             y = as.numeric(y),
             density = get_density(x, y))
    
    if (reac_pitcher$pitcher_x_direction == "Less Than" & reac_pitcher$pitcher_y_direction == "Less Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_pitcher$y_pitcher, x = reac_pitcher$x_pitcher,
             caption = "Last 10 seasons of players with at least 40 IP. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y < reac_pitcher$y_label_pitcher & data$x < reac_pitcher$x_label_pitcher,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_pitcher$pitcher_x_direction == "Less Than" & reac_pitcher$pitcher_y_direction == "Greater Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_pitcher$y_pitcher, x = reac_pitcher$x_pitcher,
             caption = "Last 10 seasons of players with at least 40 IP. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y > reac_pitcher$y_label_pitcher & data$x < reac_pitcher$x_label_pitcher,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_pitcher$pitcher_x_direction == "Greater Than" & reac_pitcher$pitcher_y_direction == "Greater Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_pitcher$y_pitcher, x = reac_pitcher$x_pitcher,
             caption = "Last 10 seasons of players with at least 40 IP. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y > reac_pitcher$y_label_pitcher & data$x > reac_pitcher$x_label_pitcher,], size = 2.5) +
        theme(legend.position="none") 
      
    } else if (reac_pitcher$pitcher_x_direction == "Greater Than" & reac_pitcher5$pitcher_y_direction == "Less Than") {
      
      ggplot(data, aes(x, y, label = name)) + 
        geom_point(aes(x, y, color = density)) + 
        labs(y = reac_pitcher$y_pitcher, x = reac_pitcher$x_pitcher,
             caption = "Last 10 seasons of players with at least 40 IP. Source: Fangraphs and baseballr package") +
        scale_color_viridis() +
        geom_text_repel(data= data[data$y < reac_pitcher$y_label_pitcher & data$x > reac_pitcher$x_label_pitcher,], size = 2.5) +
        theme(legend.position="none") 
      
    }
  },
  width = 1250, height = 700)

}

shinyApp(ui = ui, server = server)



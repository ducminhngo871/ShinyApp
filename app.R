library(shiny)
library(tidyverse)
library(babynames)
library(DT)        # for table output
library(bslib)     #for theming

players_stats_by_season_full_details <- read_delim("players_stats_by_season_full_details.csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

players_stats_by_season_full_details %>% 
  distinct(Player) %>% 
  arrange(Player) %>% 
  pull(Player)

ui <- fluidPage(
  theme = bs_theme(primary = "#123B60", 
                   secondary = "#D44420", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "sandstone"),
  # Application title
  titlePanel("NBA Basketball Players Total Points"),
  
  textInput("name", 
            "Name", 
            value = "", 
            placeholder = "Shaquille O'Neal"),
  
  # Show a plot of cumulative points for the chosen player
  # Show a table beneath
  mainPanel(
    plotOutput(outputId = "timeplot"),
    dataTableOutput(outputId = "nba_table")
  )
)

server <- function(input, output) {
  nba_smry <- reactive(players_stats_by_season_full_details %>% 
    filter(Player == input$name) %>% 
    group_by(`Season Plays`) %>% 
    summarize(season_score = sum(PTS)) %>% 
    mutate(total_score = cumsum(season_score)))
  
  output$timeplot <- renderPlot({
    nba_smry() %>% 
      ggplot(aes(x = `Season Plays`, y = total_score)) +
      geom_line() +
      geom_point() + 
      labs(title = paste("Total NBA points of", input$name),
           x = "",
           y = "") +
      theme_minimal()
  })
  
  output$nba_table <- renderDataTable(nba_smry())
}

shinyApp(ui = ui, server = server)
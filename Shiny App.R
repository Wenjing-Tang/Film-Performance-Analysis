library(shiny)
library(tidyverse)
library(lubridate)


# Load and clean data -----------------------------------------------------

ratings <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv"
  )

# Clean up some mistakes in the data
# The Whispers appears twice -- delete one of the duplicates
# Twin Peaks remake has wrong titleId --- replace with tt4093826
duplicate_row <- ratings %>%
  mutate(id = row_number()) %>%
  filter(titleId == "tt3487410") %>%
  pull(id) %>%
  .[1]
ratings <- ratings[-duplicate_row,]

wrongtitleId_row <- ratings %>%
  mutate(id = row_number()) %>%
  filter(titleId == "tt0098936", year(date) == 2017) %>%
  pull(id) %>%
  .[1]
ratings$titleId[wrongtitleId_row] <- "tt4093826"

# Find the shows that were remade (have more than 1 of a particular season)
remakes <- ratings %>%
  count(title, seasonNumber) %>%
  filter(n > 1)

# Rename shows that were remade
ratings <- ratings %>%
  group_by(titleId) %>%
  mutate(title = ifelse(title %in% remakes$title,
                        str_c(title, " (", year(min(
                          date
                        )), ")"),
                        title))

# Remove shows only have one season
ratings2 <- ratings %>%
  group_by(title) %>%
  filter(max(seasonNumber) > 1, seasonNumber < 21)

# Add month to each show
ratings3 <- ratings %>%
  mutate(month = as.numeric(month(date))) %>%
  group_by(month) %>%
  arrange(month)


# Shiny UI ----------------------------------------------------------------

# Define UI for application

ui <- fluidPage(tabsetPanel(
  tabPanel(
    "Average rating vs. Season Number",
    titlePanel("Whether a series is successful"),
    sidebarLayout(sidebarPanel(
      selectInput(
        inputId = "show",
        label = "Title",
        choices = unique(ratings2$title)
      )
    ),
    
    mainPanel(plotOutput("line_chart")))
  ),
  tabPanel(
    "All Shows' performance by month",
    titlePanel("Best time to put on a show"),
    sidebarLayout(sidebarPanel(
      radioButtons(
        inputId = "month",
        label = "Month:",
        choices = unique(ratings3$month)
      )
    ),
    
    mainPanel(plotOutput("plot")))
  ),
  tabPanel(
    "Genre Combination",
    titlePanel("Choose your genre"),
    checkboxGroupInput(
      inputId = "genres",
      label = "Genres :",
      choices = unique(unlist(lapply(ratings$genres, function(x) {
        unlist(strsplit(x, ','))
      })))
    ),
    mainPanel(DT::dataTableOutput('tbl'))
  )
))


# Shiny Server ------------------------------------------------------------

server <- function(input, output) {
  tabsetPanel(tabPanel("tab1",
                       output$line_chart <- renderPlot({
                         ratings2 %>%
                           filter(title == input$show) %>%
                           ggplot(aes(x = seasonNumber, y = av_rating)) +
                           geom_line() +
                           geom_point(color = "red",size = 2.3)+
                           scale_x_continuous(breaks = 1:50)+
                           geom_text(aes(label = av_rating),  position = "dodge", vjust = 0, size = 5)
                       })))
  
  tabPanel("tab2",
           output$plot <- renderPlot({
             ratings3 %>%
               filter(month == input$month) %>%
               ggplot(aes(x = av_rating, y = share)) +
               geom_point() +
               scale_y_log10() +
               geom_vline(xintercept = 8.115,
                          color = "red",
                          size = 1.5) +
               geom_hline(yintercept = 0.32,
                          color = "steelblue1",
                          size = 1.5)
             
           }))
  tabPanel("tab3",
           output$tbl <- DT::renderDataTable({
             select_vector = as.vector(input$genres)
             deal_f <- function(select_vector,ratings){
               if (length(select_vector)>0){
                 result = ratings
                 #index_label = FALSE
                 for (i in select_vector){
                   result = result[unlist(lapply(result$genres, function(i, x) {
                     ifelse(length(grep(i , x)) == 0, FALSE, TRUE)
                   }, i = i)),]
                 }
               }else{
                 result = ratings%>%filter(genres=='abc' )
               }
               return(result)
             }
             deal_f(select_vector,ratings)
             
           }))
}

# Run the application
shinyApp(ui = ui, server = server)
# SETUP -------------------------------------------------------------------

paquetes <- list(
  "Shiny Core" = list("shiny", "bs4Dash"),
  "Shiny Extras" = list("shinyWidgets","dashboardthemes"),
  "Plotting" = list("ggplot2","plotly"),
  "Tidyverse" = list("tidyverse", "glue"),
  "Generales" = list("readr")
)
lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         if (x %in% rownames(installed.packages()) == FALSE) {
           install.packages(x, verbose = F)
         }
         library(x, character.only = T, verbose = F)
       })
rm(list = c("paquetes"))

# Load data----------------------------------------------------------------
dataset <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Set global variables for inputs------------------------------------------

v_min_year <- min(dataset$year, na.rm = TRUE)
v_max_year <- max(dataset$year, na.rm = TRUE)

v_min_avg <- min(dataset$avg, na.rm = TRUE)
v_max_avg <- max(dataset$avg, na.rm = TRUE)

v_min_gain <- min(dataset$gain, na.rm = TRUE)
v_max_gain <- max(dataset$gain, na.rm = TRUE)

v_min_peak <- min(dataset$peak, na.rm = TRUE)
v_max_peak <- max(dataset$peak, na.rm = TRUE)


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("SteamCharts"),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "rangeyear",
                  label = "year range:",
                  min = v_min_year, 
                  max = v_max_year,
                  value = c(v_min_year,v_max_year),
                  round = TRUE,
                  ticks = FALSE,
                  sep = ""
      ),
      sliderInput(inputId = "rangeavg",
                  label = "avg range:",
                  min = v_min_avg, 
                  max = v_max_avg,
                  value = c(v_min_avg,v_max_avg),
                  round = TRUE,
                  ticks = FALSE,
                  sep = ""
      ),
      sliderInput(inputId = "rangegain",
                  label = "gain range:",
                  min = v_min_gain, 
                  max = v_max_gain,
                  value = c(v_min_gain,v_max_gain),
                  round = TRUE,
                  ticks = FALSE,
                  sep = ""
      ),
      sliderInput(inputId = "rangepeak",
                  label = "peak range:",
                  min = v_min_peak, 
                  max = v_max_peak,
                  value = c(v_min_peak,v_max_peak),
                  round = TRUE,
                  ticks = FALSE,
                  sep = ""
      ),
      textOutput(outputId = "time"),
      uiOutput("help_text"),
      
      dropdownButton(
        radioButtons(inputId = 'theme',
                     label = 'Dashboard Theme',
                     choices =  c('grey_dark',
                                  'purple_gradient'))
      ),
      uiOutput("myTheme"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "tabcard",
                  side = "left",
                  tabPanel(tabName = "Dashboard",
                           plotlyOutput("plot_by_year", width = "400px"),
                           DTOutput('tbl')
                  )
                  
      )
    )
  )
)


server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  reactiveDf <- reactive({
    dataset %>%
      filter(
        year >= input$rangeyear[1], 
        year<= input$rangeyear[2],
        avg >= input$rangeavg[1],
        avg <= input$rangeavg[2],
        gain >= input$rangegain[1],
        gain <= input$rangegain[2],
        peak >= input$rangepeak[1],
        peak <= input$rangepeak[2]
  )})
  
   output$tbl <- renderDT(
    reactiveDf(), filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE
    )
  )
  
  output$plot_by_year <- renderPlotly({
        ggplotly(
    ggplot(reactiveDf()) +
      aes(x = year) +
      geom_bar(fill = "#85f979"))
  })
  
  output$time <- renderText({
    glue("Last reload: {t}", t = Sys.time())
  })
  
  output$help_text <- renderUI({
    HTML("<br><b>Click below to change appearance</b>")
  })
  
  output$myTheme <- renderUI( shinyDashboardThemes(theme = input$theme))
  
}

shinyApp(ui = ui, server = server)
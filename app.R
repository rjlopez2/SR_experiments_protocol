# Loading pakages

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(shiny, tidyverse, plotly, 
               DT, shinythemes, shinyFiles, fs, ijtiff, plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  
  # Application title
  titlePanel("SR Ca2+ experiments protocol",
             windowTitle = "SR experiment protocol"),
  
  
  # Sidebar with a button for ex drectory selection 
  fluidRow(
    br(),
    # br(),
    column(12,
           titlePanel(h2("Path to your image directory")),
           sidebarLayout(
             sidebarPanel(
               shinyDirButton(id = "exp_dir", 
                              label = "Directory",
                              title = "Upload"),
               # actionButton("makeplot", "Make plot")
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               
               h3("This is your current working directory"),
               verbatimTextOutput("exp_dir"),
               tags$hr(),
               
               h3("your experiment profile"),
               plotlyOutput("ls_full_trace"),
               tags$hr(),
               
               plotOutput("distPlot")
             )
           )
    )
  ),
  fluidRow(
    column(12,
           actionButton("makeplot", "Make plot") 
           )
  )
  
  # fluidRow(
  #   column(12,
  #          sidebarLayout(
  #            sidebarPanel(
  #              actionButton("makeplot", "Make plot")
  #            )
  #          )
  #   )
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  shinyDirChoose(input, 'exp_dir',
                 root=volumes)
  
  exp_dir <- reactive(input$exp_dir)
  # exp_dir <- eventReactive(input$makeplot, {input$exp_dir})
  output$exp_dir <- renderText({  # use renderText instead of renderPrint
    parseDirPath(volumes, exp_dir())
  })
  
  # ls_trace_df <- reactive({
  ls_trace_df <- eventReactive(input$makeplot, {
    
    parseDirPath(volumes, exp_dir()) %>% 
      fs::dir_ls(regexp = "[0-9]{4,4}_BG.tif$") %>%
      map_dfr(~make_trace_df(.x)) %>% 
      clean_data_func() })
  
  output$ls_full_trace <- renderPlotly({
    ls_trace_df() %>% 
      make_plotly_profile()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

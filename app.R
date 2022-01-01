# Loading pakges

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(shiny, tidyverse, plotly, 
               DT, shinythemes, shinyFiles, fs, ijtiff, plotly)

# Define helper functions

# create df from plrofiles of tif ls images
make_trace_df <- function(my_file_dir){
  
  y_px_size = read_tags(my_file_dir) %>% 
    pluck("frame1", "y_resolution")
  
  my_trace_df <- my_file_dir %>%
    ijtiff::read_tif() %>%
    `[` ( , , 1, 1) %>%
    rowMeans() %>%
    dplyr::tibble(Time = 1:length(.) * y_px_size / 1000, # set the time in seconds
                  Fluorescence = .)
  
  return(my_trace_df)
}


# make plotly plot


make_plotly_profile <- function(ls_profile_df){
  {ls_profile_df %>% 
      ggplot2::ggplot(aes(x = Time, 
                          y = Fluorescence)) +
      geom_line()} %>% 
    ggplotly()
  
}

# extract metadata from a single tif file

extract_metadata_tif <- function(file_dir, meta_data){
  
  meta_data_names <- set_names(list(meta_data))
  
  metadata_df <- meta_data_names %>%
    map_dfc(~read_tags(file_dir, frames = "all") %>% 
              pluck(pluck("frame1"))) %>% 
    select(all_of(meta_data))
  
  rm(meta_data_names)
  return(metadata_df)
}

# create df profiles from multiples tif ls images concatenated

multiple_ls_profile <- function(my_dir){
  #list the tif files in the provided directory
  my_ls_file_list <- my_dir %>% 
    fs::dir_ls(regexp = "[0-9]{4,4}_BG.tif$")
  
  # create df of files metadata
  metadata <- my_ls_file_list %>% 
    map_dfr(~extract_metadata_tif(.x, 
                                  meta_data =  c("y_resolution")), 
            .id = "file_id")
  
  full_ls_df <- my_ls_file_list %>% 
    map_dfr(make_trace_df, .id = "file_id") %>% 
    dplyr::left_join(metadata, by = "file_id") %>% 
    mutate(file_id = str_sub(file_id, start = -24)) %>% 
    mutate(Time = 1:length(Time) * y_resolution * 1000) %>% 
    select(-y_resolution)
  
  rm(my_ls_file_list, metadata)
  
  return(full_ls_df)
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  
  # Application title
  titlePanel("SR Ca2+ experiments protocol",
             windowTitle = "SR experiment protocol"),
  
  
  # Sidebar with a button for ex drectory selection 
  sidebarLayout(
    sidebarPanel(
      shinyDirButton(id = "exp_dir", 
                     label = "Directory of your experiment",
                     title = "Upload"),

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

# Define server logic required to draw a histogram
server <- function(input, output) {
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  shinyDirChoose(input, 'exp_dir',
                 root=volumes)
  
  exp_dir <- reactive(input$exp_dir)
  output$exp_dir <- renderText({  # use renderText instead of renderPrint
    parseDirPath(volumes, exp_dir())
  })
  
  ls_trace_df <- reactive({parseDirPath(volumes, exp_dir()) %>% 
      multiple_ls_profile() })
  
  output$ls_full_trace <- renderPlotly({
    ls_trace_df() %>% 
      make_plotly_profile()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

ui <- fluidPage(
    
    theme = bslib::bs_theme(version=5, bootswatch = "spacelab"),
    
    # --- HEADER
    tags$head(tags$title("DATA423 Assignment")),
    HTML("
    <div style='display: flex; justify-content: space-between; align-items: center;'>
        <div>
            <h2 style='margin: 1;'>Data Explorer<sup>©</sup></h2>
        </div>
        <h3 style='margin: 1;'>Kane Williams [<i>xxxxx@uclive.ac.nz</i>]</h3>
    </div>
    "),
    
    # --- NAVBAR (TOP)
    navbarPage(
        title = "",
        tabPanel("Quickview", # --- QUICKVIEW
                 
                 # --- SIDEBAR (LEFT)
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file1", "Choose CSV File", #+ file1
                                   accept = c("text/csv",
                                              "text/comma-separated-values",
                                              "text/plain",
                                              ".csv")),
                         tags$p("Suggestions:"),
                         uiOutput("recommended_files") #- recommended_files
                     ),
                     
                     # --- MAIN PANEL (RIGHT)
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Summary", verbatimTextOutput("summary")), #- summary
                             tabPanel("Data Table", DT::dataTableOutput(outputId = "data_table")) #- data_table
                         )
                     )
                 ),
        ),
        tabPanel("Missing Values", # --- MISSING VALUES
                 
                 sidebarLayout(
                     sidebarPanel(
                         conditionalPanel(
                             condition = "input.missingValuesTabset == 'Vis-Miss Plot'",
                             selectInput("vis_miss_portion", "Display Portion", 
                                         choices = c("All Data" = "all",
                                                     "First Half (Columns)" = "first_half",
                                                     "Second Half (Columns)" = "second_half")),
                             selectInput("vis_miss_color", "Color Scheme", 
                                         choices = c("Default" = "default",
                                                     "Vibrant" = "qual",
                                                     "Colourblind Safe" = "cb_safe")),
                             checkboxInput("vis_miss_sort", "Sort by Missing", value = TRUE)

                         ),
                         conditionalPanel(
                             condition = "input.missingValuesTabset == 'Null Count'",
                             # Add controls for Null Count plot here
                         ),
                         conditionalPanel(
                             condition = "input.missingValuesTabset == 'Upset Chart'",
                             sliderInput("upset_nintersects", "Number of Intersections", 
                                         min = 1, max = 50, value = 10),
                             sliderInput("upset_nsets", "Number of Sets", 
                                         min = 1, max = 20, value = 5),
                             checkboxInput("upset_order_by", "Order by Intersection Size", value = TRUE)
                         ),
                         conditionalPanel(
                             condition = "input.missingValuesTabset == 'Rising-Value Chart'",
                             sliderInput("rising_value_percent", "Select Percentage of Variables:", 
                                         min = 0, max = 100, value = 100, step = 10),
                             div(style = "max-height: 400px; overflow-y: auto; column-count: 3; column-gap: 20px;",
                                 checkboxGroupInput("rising_value_vars", "Select Variables:", 
                                                    choices = NULL,
                                                    selected = NULL)
                             ),
                             checkboxInput("rising_value_scale", "Scale Variables", value = TRUE),
                             checkboxInput("rising_value_center", "Center Variables", value = TRUE),
                             actionButton("select_all_rising_value", "Select All"),
                             actionButton("deselect_all_rising_value", "Deselect All")
                         )
                     ),
                     mainPanel(
                         tabsetPanel(id = "missingValuesTabset",
                             tabPanel("Vis-Miss Plot", plotOutput("vis_miss_plot", height = "calc(100vh - 100px)")),
                             tabPanel("Upset Chart", plotOutput("upset_chart", height = "calc(100vh - 100px)")),
                             tabPanel("Rising-Value Chart", plotOutput("rising_value_chart", height = "calc(100vh - 100px)")),
                             tabPanel("Null Count"),
                         )
                     ),
                 ),
        ),
        tabPanel("Similarities",
        
                sidebarLayout(
                    sidebarPanel(
                        actionButton("TEST2", "placeholder TEST3")
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Corrgram"),
                            tabPanel("Hierarchy Chart"),
                        )
                    ),
                ),
        ),
        tabPanel("Distributions",
                 
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("TEST3", "placeholder TEST3")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Box Plot"),
                             tabPanel("Histogram"),
                         )
                     ),
                 ),
        ),
        tabPanel("Relationships",
                 
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("TEST4", "placeholder TEST4")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Scatter Plot"),
                             tabPanel("Pair Plot"),
                             tabPanel("Mosaic Plot"),
                         )
                     ),
                 ),
        ),
        navbarMenu("More", 
            tabPanel("Options",
                     
                     uiOutput("options_page") #- options
                     
                     ),
            tabPanel("About",
                     
                     uiOutput("about_page") #- about
                     
                     )
            ),
        nav_spacer(),
        # nav_item(
        #     input_dark_mode(id = "dark_mode", mode = "light")
        # )
    ),


    
    # tags$footer(
    #     style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: #f5f5f5; text-align: center; padding: 5px; font-size: 0.8em;",
    #     HTML("© Kane Williams")
    # )
)

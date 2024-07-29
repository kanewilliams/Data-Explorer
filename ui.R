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
            <h2 style='margin: 1;'>Data<sup>Explorer</sup></h2>
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
                         wellPanel(
                             fileInput("file1", "Choose CSV File", #+ file1
                                       accept = c("text/csv",
                                                  "text/comma-separated-values",
                                                  "text/plain",
                                                  ".csv")),
                             tags$p("Suggestions:"),
                             uiOutput("recommended_files"), #- recommended_files
                         ),
                         
                         # CLEAN CSV
                         conditionalPanel(
                             condition = "output.file_uploaded",
                             fileInput("cleaning_script", "Upload Cleaning Script",
                                       accept = c("text/r",
                                                  "text/plain",
                                                  ".r", ".R", ".clean")),
                             uiOutput("cleaning_info"),
                         )
                     ),
                     
                     # --- MAIN PANEL (RIGHT)
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Summary", withSpinner(verbatimTextOutput("summary"))), #- summary
                             tabPanel("Data Table", withSpinner(dataTableOutput(outputId = "data_table"))), #- data_table
                             tabPanel("dfSummary", withSpinner(htmlOutput(outputId = "dfsummary"))) #- data_table
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
                             # selectInput("vis_miss_color", "Color Scheme", # For vis_dat() only
                             #             choices = c("Default" = "default",
                             #                         "Vibrant" = "qual",
                             #                         "Colourblind Safe" = "cb_safe")),
                             checkboxInput("vis_miss_sort", "Sort by Missing", value = FALSE),
                             checkboxInput("vis_miss_cluster", "Cluster Missing", value = FALSE)

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
                             selectInput("facet_by", "Facet by: (TODO)", choices = NULL, multiple = FALSE),
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
                             actionButton("deselect_all_rising_value", "Deselect All"),
                         ),
                         # conditionalPanel(
                         #     condition = "input.missingValuesTabset == 'Null Count'",
                         #     verbatimTextOutput("description_null_count")
                         # )
                     ),
                     mainPanel(
                         tabsetPanel(id = "missingValuesTabset",
                             tabPanel("Vis-Miss Plot", withSpinner(plotOutput("vis_miss_plot", height = "calc(100vh - 150px)"))),
                             tabPanel("Upset Chart", withSpinner(plotOutput("upset_chart", height = "calc(100vh - 150px)"))),
                             tabPanel("Rising-Value Chart", withSpinner(plotOutput("rising_value_chart", height = "calc(100vh - 150px)"))),
                             #tabPanel("Null Count", withSpinner(tableOutput("null_count_table"))),
                         )
                     ),
                 ),
        ),
        tabPanel("Similarities", # --- SIMILARITIES
        
                sidebarLayout(
                    sidebarPanel(
                        conditionalPanel(
                            condition = "input.similaritiesTabset == 'Corrgram'",
                            checkboxInput(inputId = "corr_abs", label = "Uses absolute correlation", value = TRUE),
                            selectInput(inputId = "corr_method", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                            selectInput(inputId = "corr_group_method", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                            ),
                        conditionalPanel(
                            condition = "input.similaritiesTabset == 'HierarchyChart'",
                            checkboxInput(inputId = "corr_abs", label = "PLACEHOLDER", value = TRUE),
                            selectInput(inputId = "corr_method", label = "PLACEHOLDER", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                            selectInput(inputId = "corr_group_method", label = "PLACEHOLDER", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                        ),
                    ),
                    
                    mainPanel(
                        tabsetPanel(id = "similaritiesTabset",
                            tabPanel("Corrgram", withSpinner(plotOutput("corrgram_plot"))),
                            tabPanel("Hierarchy Chart", withSpinner(verbatimTextOutput("hierarchy_chart"))),
                        )
                    ),
                ),
        ),
        tabPanel("Distributions", # --- DISTRIBUTIONS
                 
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
        tabPanel("Relationships", # --- RELATIONSHIPS
                 
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

# Kane Williams 2024 
# https://github.com/kanewilliams/Data-Explorer

ui <- fluidPage(
    
    theme = bslib::bs_theme(version=5, bootswatch = "spacelab"),
    
    # --- HEADER
    tags$head(tags$title("DATA423 Assignment")),
    HTML("
    <div style='display: flex; justify-content: space-between; align-items: center;'>
        <div>
            <h2 style='margin: 1;'>Data<sup>Explorer</sup></h2>
        </div>
        <h3 style='margin: 1;'>Kane Williams [<i>xxxxx@yyy.com</i>]</h3>
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
                         conditionalPanel( ### UPSET CHART
                             condition = "input.missingValuesTabset == 'Upset Chart'",
                             sliderInput("upset_nintersects", "Number of Intersections", 
                                         min = 1, max = 50, value = 10),
                             sliderInput("upset_nsets", "Number of Sets", 
                                         min = 1, max = 20, value = 5),
                             checkboxInput("upset_order_by", "Order by Intersection Size", value = TRUE),
                             tags$p("TODO: Add Title to this chart. (Surprisingly difficult! As Upset() is not a ggplot() object!)", 
                                    style = "color: #888; font-style: italic;")                         ),
                         
                         
                         conditionalPanel( ### RISING-VALUE
                             condition = "input.missingValuesTabset == 'Rising-Value Chart'",
                             #selectInput("facet_by", "Facet by: (TODO)", choices = NULL, multiple = FALSE),
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
                             tabPanel("Vis-Miss Plot", withSpinner(plotOutput("vis_miss_plot", height = "calc(100vh - 200px)"))),
                             tabPanel("Upset Chart", withSpinner(plotOutput("upset_chart", height = "calc(100vh - 200px)"))),
                             tabPanel("Rising-Value Chart", withSpinner(plotOutput("rising_value_chart", height = "calc(100vh - 200px)"))),
                             #tabPanel("Null Count", withSpinner(tableOutput("null_count_table"))),
                         )
                     ),
                 ),
        ),
        tabPanel("Similarities", # --- SIMILARITIES
        
                sidebarLayout(
                    sidebarPanel(
                        conditionalPanel(
                            condition = "input.similaritiesTabset == 'Corrgram'", ### CORRGRAM
                            sliderInput("corrgram_percent", "Select Percentage of Variables:", 
                                        min = 0, max = 100, value = 20, step = 10),
                            div(style = "max-height: 400px; overflow-y: auto; column-count: 3; column-gap: 20px;",
                                checkboxGroupInput("corrgram_vars", "Select Variables:", 
                                                   choices = NULL,
                                                   selected = NULL)
                            ),
                            actionButton("select_all_corrgram", "Select All"),
                            actionButton("deselect_all_corrgram", "Deselect All"),
                            tags$hr(),
                            tags$div(style = "margin-top: 15px;",
                                     checkboxInput(inputId = "corr_abs", label = "Use Absolute Correlation", value = TRUE)
                            ),
                            checkboxInput("corr_pie", "Use Pie Charts in Upper Panel", value = FALSE),
                            selectInput(inputId = "corr_method", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                            selectInput(inputId = "corr_group_method", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
                            tags$hr(),
                            sliderInput("corrgram_text_size", "Text Size", min = 0.2, max = 3, value = 3, step = 0.2)
                            ),
                        
                        conditionalPanel(
                            condition = "input.similaritiesTabset == 'HierarchyChart'", ### HIERARCHY CHART
                            checkboxInput(inputId = "corr_abs", label = "PLACEHOLDER", value = TRUE),
                            selectInput(inputId = "corr_method", label = "PLACEHOLDER", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                            selectInput(inputId = "corr_group_method", label = "PLACEHOLDER", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                        ),
                    ),
                    
                    mainPanel(
                        tabsetPanel(id = "similaritiesTabset",
                            tabPanel("Corrgram", withSpinner(plotOutput("corrgram_plot", height = "calc(100vh - 200px)"))),
                            tabPanel("Hierarchy Chart (Not Implemented)", withSpinner(verbatimTextOutput("hierarchy_chart"))),
                        )
                    ),
                ),
        ),
        tabPanel("Distributions", # --- DISTRIBUTIONS
                 
                 sidebarLayout(
                     sidebarPanel(
                         conditionalPanel(
                             condition = "input.distributionsTabset == 'Box Plot'", ### BOX PLOT
                             selectInput("boxplot_portion", "Display Portion", 
                                         choices = c("All Data" = "all",
                                                     "First Half (Columns)" = "first_half",
                                                     "Second Half (Columns)" = "second_half")),
                             checkboxInput(inputId = "boxplot_scale", label = "Scale Variables", value = FALSE),
                             checkboxInput(inputId = "boxplot_center", label = "Center Variables", value = TRUE),
                             checkboxInput(inputId = "boxplot_outliers", label = "Show Outliers", value = TRUE),
                             sliderInput(inputId = "boxplot_iqr", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                         ),
                         conditionalPanel(
                             condition = "input.distributionsTabset == 'Histogram'", ### HISTOGRAM
                             checkboxInput(inputId = "corr_abs", label = "AAAA", value = TRUE),
                             selectInput(inputId = "corr_method", label = "PLACEHOLDER", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                             selectInput(inputId = "corr_group_method", label = "PLACEHOLDER", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                         ),
                     ),
                         
                     mainPanel(
                         tabsetPanel(id = "distributionsTabset",
                             tabPanel("Box Plot", withSpinner(plotOutput("boxplot", height = "calc(100vh - 200px)"))),
                             tabPanel("Histogram (Not Implemented)", withSpinner(plotOutput("histogram", height = "calc(100vh - 200px)"))),
                         )
                     ),
                 ),
        ),
        tabPanel("Relationships", # --- RELATIONSHIPS
                 
                 sidebarLayout(
                     sidebarPanel(
                         conditionalPanel(
                             condition = "input.relationshipsTabset == 'Time Series Plot'", ### TIME SERIES
                             selectInput(inputId = "ts_dependent_var", 
                                         label = "Choose Dependent Variable", 
                                         choices = NULL),
                             selectInput(inputId = "ts_datetime_var", 
                                         label = "Choose DateTime Variable", 
                                         choices = NULL),
                             tags$hr(),
                             actionButton(inputId = "autoplot_ts", 
                                          label = "Plot All Numerical Variables",
                                          class = "btn-primary"),
                             actionButton(inputId = "reset_ts", 
                                          label = "Reset to Single Plot",
                                          class = "btn-secondary",
                                          style = "margin-left: 10px;"),
                             tags$p(tags$strong("Note:", style = "color: blue;"), 
                                    "Plotting all variables may take a moment for large datasets.",
                                    style = "margin-top: 5px; font-style: italic;")
                         ),
                         
                         conditionalPanel(
                             condition = "input.relationshipsTabset == 'Pairs Plot'", ### PAIRS PLOT
                             actionButton(inputId = "pairs_generate", label = "Generate Pairs Plot"),
                             tags$p(tags$strong("Warning:", style = "color: orange;"), "May take a while to Generate.", 
                                    style = "margin-top: 10px; margin-bottom: 5px;"),
                             tags$hr(style = "margin-top: 15px; margin-bottom: 15px;"),
                             actionButton(inputId = "random_select_pairs", label = "Randomly select 5"),
                             div(style = "max-height: 400px; overflow-y: auto; column-count: 3; column-gap: 20px;",
                                 checkboxGroupInput("pairs_vars", "Select Variables:", 
                                                    choices = NULL,
                                                    selected = NULL)
                             ),
                             actionButton(inputId = "select_all_pairs", label = "Select All"),
                             actionButton(inputId = "deselect_all_pairs", label = "Deselect All"),
                             tags$hr(style = "margin-top: 15px; margin-bottom: 15px;"),
                             selectInput(inputId = "pairs_colour_by", 
                                         label = "Colour by:", 
                                         choices = NULL),
                             sliderInput(inputId = "pairs_text_size", 
                                         label = "Outer Text Size:", 
                                         min = 6, max = 20, value = 14, step = 1),
                             sliderInput(inputId = "pairs_inner_text_size", 
                                         label = "Inner Text Size:", 
                                         min = 0.5, max = 10, value = 6, step = 0.5)
                         ),
                         
                         conditionalPanel(
                             condition = "input.relationshipsTabset == 'Mosaic Plot'", ### MOSAIC PLOT
                             selectizeInput(inputId = "mosaic_variables", 
                                            label = "Select variables for Mosaic Plot:", 
                                            choices = NULL, 
                                            multiple = TRUE, 
                                            options = list(maxItems = 5)),
                             checkboxInput(inputId = "mosaic_split_vertical", label = "Split Vertical", value = FALSE),
                             ),
                     ),
                     
                     mainPanel(
                         tabsetPanel(id = "relationshipsTabset",
                             tabPanel("Time Series Plot", withSpinner(plotOutput("timeseries_plot", height = "calc(100vh - 200px)"))),
                             tabPanel("Pairs Plot", withSpinner(plotOutput("pairs_plot", height = "calc(100vh - 200px)"))),
                             tabPanel("Mosaic Plot", withSpinner(plotOutput("mosaic_plot", height = "calc(100vh - 200px)"))),
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

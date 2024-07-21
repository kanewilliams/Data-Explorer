# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

ui <- fluidPage(
    
    theme = bslib::bs_theme(version=5, bootswatch = "materia"),
    
    # --- HEADER
    tags$head(tags$title("DATA423 Assignment")),
    HTML("
    <div style='display: flex; justify-content: space-between; align-items: center;'>
        <div>
            <h2 style='margin: 1;'>Data Explorer<sup>©</sup></h2>
        </div>
        <h3 style='margin: 1;'>Kane Williams [<i>pkw21@uclive.ac.nz</i>]</h3>
    </div>
    "),
    
    # --- NAVBAR (TOP)
    navbarPage(
        title = "Demo Navbar",
        tabPanel("Quickview"),
        tabPanel("Dashboard"),
        tabPanel("Options"),
        tabPanel("About")
    ),

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
        
        # Main panel for displaying summaries and plots
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", verbatimTextOutput("summary")), #- summary
                tabPanel("Plot", plotOutput("plot")), #- plot
                tabPanel("Missingness"),
                tabPanel("Pairs Plot")
            )
        )
    ),
    
    # tags$footer(
    #     style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: #f5f5f5; text-align: center; padding: 5px; font-size: 0.8em;",
    #     HTML("© Kane Williams")
    # )
)

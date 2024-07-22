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
        tabPanel("Quickview",
                 
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
                             tabPanel("Data Table", DT::dataTableOutput(outputId = "data_table")), #- data_table
                             tabPanel("Null-Count"),
                             tabPanel("Vis_Miss")
                         )
                     )
                 ),
                 
                 ),
        tabPanel("Missing Values"),
        tabPanel("Similarities"),
        tabPanel("Distributions"),
        tabPanel("Relationships"),
        navbarMenu("More", 
            tabPanel("Options",
                     
                     verbatimTextOutput("options_page") #- options
                     
                     ),
            tabPanel("About",
                     
                     uiOutput("about_page") #- about
                     
                     )
            )
    ),


    
    # tags$footer(
    #     style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: #f5f5f5; text-align: center; padding: 5px; font-size: 0.8em;",
    #     HTML("© Kane Williams")
    # )
)

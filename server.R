# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

server <- function(input, output, session) {
  
    csv_files <- reactive({
      list.files(pattern = "\\.csv$", recursive = TRUE)
    })
    
    # List Suggested Files
    output$recommended_files <- renderUI({ #- recommended_files
      files <- csv_files()
      if (length(files) > 0) {
        tagList(
          lapply(files, function(file) {
            tags$div(
              style = "margin-bottom: 5px;",
              actionLink(inputId = paste0("file_", gsub("[^a-zA-Z0-9]", "_", file)), 
                         label = file) # e.g. data/example.csv -> file_data_example
            )
          })
        )
      } else {
        tags$p("No CSV files found in the current directory.")
      }
    })
    
    # Read CSV file
    data <- reactive({
      selected <- selected_file()
      
      if (!is.null(selected)) {
        if (file.exists(selected)) {
          read.csv(selected, header = TRUE)
        } else {
          NULL
        }
      } else if (!is.null(input$file1$datapath)) {
        read.csv(input$file1$datapath, header = TRUE)
      } else {
        NULL
      }
    })
    
  
    selected_file <- reactiveVal(NULL)
    
    observeEvent(csv_files(), {
      lapply(csv_files(), function(file) {
        observeEvent(input[[paste0("file_", gsub("[^a-zA-Z0-9]", "_", file))]], {
          selected_file(file)
        })
      })
    })
  
    observeEvent(input$file1, {
      if (!is.null(input$file1$datapath)) {
        selected_file(input$file1$datapath)
      }
    })
    
    
# --- Main Panel Outputs ---
    
    output$summary <- renderPrint({ #+ summary
      data <- data()
      if (is.null(data)) {
        return("No data loaded. Please select or upload a CSV file.")
      }
      summary(data)
    })
    
    output$plot <- renderPlot({ #+ plot
        data <- data() 
        if (is.null(data)) {
            return(NULL)
        }
        
        # TODO update
        hist(data$Y, main = "Histogram of data$Y", xlab = "Value")
    })
}
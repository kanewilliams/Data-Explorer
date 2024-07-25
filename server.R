# Kane Williams 2024 
# Data423-24S2 Assignment 1
# Contact: (pkw21@uclive.ac.nz)

server <- function(input, output, session) {
  
### QUICK VIEW ###
  
    # --- SIDEBAR (LEFT): CSV File Logic ---
    csv_files <- reactive({
      list.files(pattern = "\\.csv$", recursive = TRUE)
    })
    
    # Display Suggested Files
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
    
    selected_file <- reactiveVal(NULL)
    
    # Suggested File Selected
    observeEvent(csv_files(), {
      lapply(csv_files(), function(file) {
        observeEvent(input[[paste0("file_", gsub("[^a-zA-Z0-9]", "_", file))]], {
          selected_file(file)
        })
      })
    })
    
    # Manual File Selected
    observeEvent(input$file1, {
      if (!is.null(input$file1$datapath)) {
        selected_file(input$file1$datapath)
      }
    })
    
    # Read CSV file
    data <- reactive({
      selected <- selected_file()
      
      if (!is.null(selected) && file.exists(selected)) {
        read.csv(selected, header = TRUE,  stringsAsFactors=TRUE)
      } else {
        NULL
      }
    })
    
    # --- MAIN PANEL (RIGHT) ---
    
    output$summary <- renderPrint({ #+ summary
      data <- data()
      if (is.null(data)) {
        return("No data loaded. Please select or upload a CSV file.")
      }
      summary(data)
    })
    
    output$data_table <- DT::renderDataTable({ #+- data_table
      data <- data()
      if (is.null(data)) {
        # TODO return proper error message
        return("No data loaded. Please select or upload a CSV file.")
      }
      
      DT::datatable(data = data,
                    options = list(searching = TRUE,
                                   pageLength = 10,
                                   lengthMenu = c(5, 10, 100),
                                   ordering = input$Order,
                                   scrollX = TRUE
                    ),
                    # Clickable Rows
                    #extensions = list(Responsive = TRUE)
      )
    })

### MISSING VALUES ###
    
    ### --- Vis_Miss Plot
    output$vis_miss_plot <- renderPlot({
      req(data())
      data_to_plot <- switch(input$vis_miss_portion,
                             "all" = data(),
                             "first_half" = data() %>% select(1:(ncol(.)/3)), # TODO FIX: BROKEN
                             "second_half" = data() %>% select((ncol(.)/2 + 1):ncol(.))
      )
      vis_dat(data_to_plot, palette = input$vis_miss_color,
              sort_type = input$vis_miss_sort)
    })
    
    ### --- Upset Chart
    output$upset_chart <- renderPlot({
      req(data())
      
      # Create a list of sets, where each set contains row indices of missing values
      missing_sets <- lapply(names(data()), function(col) {
        which(is.na(data()[[col]]))
      })
      names(missing_sets) <- names(data())
      
      # Filter out sets (variables) with no missing values
      missing_sets <- missing_sets[sapply(missing_sets, length) > 0]
      
      # Ensure we have at least two sets with missing values
      if(length(missing_sets) < 2) {
        return(plot.new() + 
                 text(0.5, 0.5, "Not enough variables with missing values for an UpSet plot"))
      }
      
      upset(fromList(missing_sets), 
            nsets = min(input$upset_nsets, length(missing_sets)),
            nintersects = input$upset_nintersects,
            order.by = if(input$upset_order_by) "freq" else "degree",
            main.bar.color = "darkblue",
            sets.bar.color = "darkred",
            matrix.color = "black",
            shade.color = "lightgray")
    })
    
    ### -- Rising Value Chart
    # NOTE: Works only on CONTINUOUS COLUMNS
    output$rising_value_chart <- renderPlot({
      req(data())
      
      data <- data()[, sapply(data(), function(col) is.numeric(col))] # select numeric
      for (col in 1:ncol(data)) {
        data[,col] <- data[order(data[,col]),col, drop = FALSE] #sort each column in ascending order
      }
      
      data_filtered <- data[, input$rising_value_vars, drop = FALSE] # choose columns by rising_value_var
      
      data <- scale(x = data_filtered, center = input$rising_value_center, scale = input$rising_value_scale)
      mypalette <- rainbow(ncol(data))
      matplot(x = seq(1, 100, length.out = nrow(data)), y = data, type = "l", xlab = "Percentile (%)", ylab = "Standardised Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
      legend(legend = colnames(data), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(data)^0.3))
    })
    
    # Automatically update groupcheckbox for column selection
    observe({
      req(data())
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      updateCheckboxGroupInput(session, "rising_value_vars", 
                               choices = numeric_cols,
                               selected = numeric_cols
                               )
    })
    
    # Select All button functionality
    observeEvent(input$select_all_rising_value, {
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      updateCheckboxGroupInput(session, "rising_value_vars", 
                               selected = numeric_cols)
    })
    
    # Deselect All button functionality
    observeEvent(input$deselect_all_rising_value, {
      updateCheckboxGroupInput(session, "rising_value_vars", 
                               selected = character(0))
    })
    
### OPTIONS ###
    
    output$options_page <- renderUI({
      fluidPage(
        tags$head(
          tags$style(HTML("
        .options-section { margin-bottom: 30px; }
        .option-item { margin-bottom: 15px; }
        .centered-content {
          max-width: 500px;
          margin: 0 auto;
        }
        .btn-block {
          width: 100%;
        }
      "))
        ),
        
        h2("Options (experimental)", align = "center"),
        
        fluidRow(
          column(6, offset = 3,
                 div(class = "centered-content",
                     div(class = "options-section",
                         div(class = "option-item",
                             checkboxInput("dark_mode", "Enable Dark Mode (click icon below)", value = FALSE)
                         ),
                         input_dark_mode(id = "dark_mode", mode = "light"),
                         div(class = "option-item",
                             selectInput("theme_color", "Theme Color", 
                                         choices = c("Default", "Blue", "Green", "Purple", "Orange"))
                         ),
                         div(class = "option-item",
                             sliderInput("font_size", "Font Size", min = 12, max = 24, value = 16, step = 1)
                         )
                     ),
                     actionButton("save_options", "Save Options", class = "btn-primary btn-block")
                 )
          )
        )
      )
    })
    
    
### ABOUT ###    
    
    # --- Title ---
    output$about_page <- renderUI({
      fluidPage(
        tags$head(
          tags$style(HTML("
        .centered-title { text-align: center; margin-bottom: 30px; }
        .content-section { margin-bottom: 20px; }
      "))
        ),
        
        div(class = "centered-title",
            h1("About Data Explorer")
        ),
        
    # --- Main Content ---
        fluidRow(
          column(8,
                 div(class = "content-section",
                     h3("Purpose"),
                     p("Data Explorer is a web application designed for quickly exploring CSV datasets, made entirely in R shiny."),
                     p("It provides various tools for data visualization, summary statistics, and exploratory data analysis, and was created as part of a project for a course (DATA423: Data Science in Industry) at the University of Canterbury, New Zealand."),
                     p("For more information and to view the source code, visit the ", 
                       a("GitHub repository", href = "https://github.com/kanewilliams/Data-Explorer", target = "_blank"), ".")
                 ),
                 fluidRow(
                   column(6,
                          div(class = "content-section",
                              h3("Features"),
                              tags$ul(
                                tags$li("Automatic CSV detection"),
                                tags$li("Interactive data tables"),
                                tags$li("Missing value analysis"),
                                tags$li("Various data visualisations")
                              )
                          )
                   ),
                   column(6,
                          div(class = "content-section",
                              h3("How to Use"),
                              tags$ol(
                                tags$li("Upload a CSV file using the file input on the Quickview page."),
                                tags$li("Navigate through different tabs to explore various aspects of your data."),
                                tags$li("Use interactive features to customize your analysis.")
                              )
                          )
                   )
                 )
          ),
          column(4,
                 div(class = "content-section",
                     h3("Developer Information"),
                     p(strong("Name:"), "Kane Williams"),
                     p(strong("Contact:"), a("pkw21@uclive.ac.nz", href="mailto:pkw21@uclive.ac.nz"))
                 ),
                 div(class = "content-section",
                     h3("Acknowledgements"),
                     p("Special thanks to the University of Canterbury and the DATA423 course instructors for their guidance and support."),
                     p("Some code (particularly for the visualisations) was modified from code provided in the course. (Authors: Nick Ward, Phil Davies, Nicki Cartlidge)")
                 ),
                 div(class = "content-section",
                     h3("Technologies Used"),
                     tags$ul(
                       tags$li("R"),
                       tags$li("Shiny"),
                       tags$li("Various R packages (e.g. thematic, ragg, DT, ggplot2, dplyr)")
                     )
                 )
          )
        ),
        
        hr(),
        
        fluidRow(
          column(12,
                 p(em("© 2024 Kane Williams. All rights reserved."))
          )
        )
      )
    })
}
# Kane Williams 2024 
# https://github.com/kanewilliams/Data-Explorer

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
    file_uploaded <- reactiveVal(FALSE)
    data <- reactiveVal(NULL)
    
    # Suggested File Selected
    observeEvent(csv_files(), {
      lapply(csv_files(), function(file) {
        observeEvent(input[[paste0("file_", gsub("[^a-zA-Z0-9]", "_", file))]], {
          selected_file(file)
          file_uploaded(TRUE)
        })
      })
    })
    
    # Manual File Selected
    observeEvent(input$file1, {
      if (!is.null(input$file1$datapath)) {
        selected_file(input$file1$datapath)
        file_uploaded(TRUE)
      }
    })
    
    # Read CSV file after Selection
    observeEvent(selected_file(), {
      selected <- selected_file()
      
      if (!is.null(selected) && file.exists(selected)) {
        df <- read.csv(selected, header = TRUE, stringsAsFactors=TRUE)
        data(df)
      }
    })
    
    
    # Show Cleaning Box when file Uploaded
    output$file_uploaded <- reactive({
      file_uploaded()
    })
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    ### - CLEANING
    observeEvent(input$cleaning_script, {
      req(input$cleaning_script)
      req(data())
      
      script <- readLines(input$cleaning_script$datapath)
      comments <- script[grepl("^#", script)] # Find comments in .clean
      comments <- gsub("^#\\s*", "", comments)  # Remove leading '#'
      
      # Create env$data as temporary
      env <- new.env()
      env$data <- data()
      
      # Clean env$data
      tryCatch({
        eval(parse(text = script), envir = env)
        data(env$data)  # Update Data
        showNotification("Cleaning script applied successfully", type = "message")
        
        output$cleaning_info <- renderUI({
          tagList(
            h4("Applied Cleaning Actions:"),
            tags$pre(paste(comments, collapse = "\n"))
          )
        })

      }, error = function(e) {
        showNotification(paste("Error in cleaning script:", e$message), type = "error")
      })
    })
    
    
    # --- MAIN PANEL (RIGHT) ---
    
    output$summary <- renderPrint({ #+ summary
      req(data())
      summary(data())
    })
    
    output$data_table <- DT::renderDataTable({ #+- data_table
      req(data())
      DT::datatable(data = data(),
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
    
    output$dfsummary <- renderPrint({ #+ dfsummary
      req(data())
      data() %>%
        summarytools::dfSummary(col.widths = c(10,80,150,120,120,180,220)) %>% 
        summarytools::view(, method = "render", headings = FALSE, bootstrap.css = FALSE)
    })

### MISSING VALUES ###
    
      ### --- Vis_Miss Plot
    output$vis_miss_plot <- renderPlot({
      req(data())
      data_to_plot <- switch(input$vis_miss_portion,
                             "all" = data(),
                             "first_half" = data() %>% select(1:floor(ncol(.)/2)),
                             "second_half" = data() %>% select((floor(ncol(.)/2) + 1):ncol(.))
      )
      
      # Dynamic Title
      vis_miss_title <- reactive({
        sorting <- if(input$vis_miss_sort) "Sorted by Missing" else "Unsorted"
        clustering <- if(input$vis_miss_cluster) "Clustered" else "Unclustered"
        
        paste0("Vis-Miss Plot (", 
               sorting, ", ", 
               clustering, ")")
      })
      
      vis_miss(data_to_plot, 
               sort_miss = input$vis_miss_sort, 
               cluster = input$vis_miss_cluster) +
        
        ggtitle(vis_miss_title()) +
        
        theme(
          axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 12),
          #axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 25, face = "bold", hjust = 0.5, vjust=-2,
                                    margin = margin(b = 20)),
          plot.title.position = "plot"
        )
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
      
      # Dynamic Title
      title <- paste0("UpSet Plot of Missing Values",
                      if(input$upset_order_by) "- Ordered by Intersection Size")
    
    # Create the UpSet plot
    upset(fromList(missing_sets), 
          nsets = min(input$upset_nsets, length(missing_sets)),
          nintersects = input$upset_nintersects,
          order.by = if(input$upset_order_by) "freq" else "degree",
          text.scale = c(2.0), # Increase text size
          main.bar.color = "darkblue",
          sets.bar.color = "darkred",
          matrix.color = "black",
          shade.color = "lightgray"
    )
    
    # TODO ADD TITLE, HOW??
    
  })

    ### -- Rising Value Chart
    # NOTE: Works only on CONTINUOUS features
    
    output$rising_value_chart <- renderPlot({
      req(data())
      
      # Selects Numeric features
      data <- data()[, sapply(data(), function(col) {
                  is.numeric(col)
              })]
      
      for (col in 1:ncol(data)) {
        data[,col] <- data[order(data[,col]),col, drop = FALSE] # sort each column in ascending order
      }
      
      data_filtered <- data[, input$rising_value_vars, drop = FALSE] # choose columns by rising_value_var
      
      # Dynamic Title
      rising_value_title <- paste0("Rising Value Chart (", 
                                   if(input$rising_value_center && input$rising_value_scale) {
                                     "Centered, Scaled"
                                   } else if(input$rising_value_center) {
                                     "Centered, Not Scaled"
                                   } else if(input$rising_value_scale) {
                                     "Not Centered, Scaled"
                                   } else {
                                     "Not Centered, Not Scaled"
                                   },
                                   ")")
      
      data <- scale(x = data_filtered, center = input$rising_value_center, scale = input$rising_value_scale)
      mypalette <- rainbow(ncol(data))
      matplot(x = seq(1, 100, length.out = nrow(data)), y = data, type = "l", xlab = "Percentile (%)", ylab = "Standardised Values", lty = 1,
              lwd = 1, col = mypalette, main = rising_value_title)
      legend(legend = colnames(data), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(data)^0.3))
    })
    
    # Update facet_by choices
    observe({
      req(data())
      categorical_cols <- names(data())[sapply(data(), function(x) is.factor(x) || (is.numeric(x) && length(unique(x)) <= 10))]
      updateSelectInput(session, "facet_by", 
                        choices = c("None", categorical_cols),
                        selected = "None")
    })
    
    # Update selected variables based on percentage slider
    observeEvent(input$rising_value_percent, {
      req(data())
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      num_to_select <- ceiling(length(numeric_cols) * input$rising_value_percent / 100)
      selected_cols <- numeric_cols[1:num_to_select]
      updateCheckboxGroupInput(session, "rising_value_vars", 
                               selected = selected_cols)
    })
    
    # Update group check box based on availability columns (on data load)
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
    
    ### --- Null Count
    null_counts <- reactive({
      req(data())
      sapply(data(), function(x) sum(is.na(x)))
    })
    
    output$null_count_table <- renderTable({
      counts <- null_counts()
      data.frame(
        Variable = names(counts),
        'Missing Values' = counts,
        check.names = FALSE
      )
    })
    
    output$description_null_count <- renderText({
      "This table provides a quick, simple overview of missing value counts."
    })
    
### SIMILARITIES ###
    
    ### -- CORRGRAM
    
    selected_corrgram_data <- reactive({
      req(data(), input$corrgram_vars)
      data()[, input$corrgram_vars, drop = FALSE]
    })
    
    # Initialize selected variables based on default percentage
    observe({
      req(data())
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      updateCheckboxGroupInput(session, "corrgram_vars", 
                               choices = numeric_cols,
                               selected = character(0))  # Start with none selected
    })
    
    # React to both initial load and subsequent changes of the percentage slider
    observeEvent(list(data(), input$corrgram_percent), {
      req(data())
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      num_to_select <- ceiling(length(numeric_cols) * input$corrgram_percent / 100)
      selected_cols <- numeric_cols[1:num_to_select]
      updateCheckboxGroupInput(session, "corrgram_vars", 
                               selected = selected_cols)
    }, ignoreInit = FALSE)  # This ensures it runs on initialization
    
    # Render Corrgram
    output$corrgram_plot <- renderPlot({
      req(selected_corrgram_data())
      
      # Dynamic Title
      corrgram_title <- paste0(
        "Corrgram (",
        if(input$corr_abs) "Absolute Correlation Enabled, " else "Absolute Correlation Disabled, ",
        "Method: ", input$corr_method, ", ",
        "Grouping: ", if(input$corr_group_method == FALSE) "None" else input$corr_group_method,
        ")"
      )
      
      text_size <- input$corrgram_text_size
      upper_panel <- if(input$corr_pie) panel.pie else panel.shade
      
      corrgram(selected_corrgram_data(), 
               order = input$corr_group_method, 
               abs = input$corr_abs, 
               cor.method = input$corr_method,
               main = corrgram_title,
               cex.labels = text_size,
               lower.panel = panel.shade,
               upper.panel = upper_panel
      )
    })
    
    # Select All button functionality
    observeEvent(input$select_all_corrgram, {
      numeric_cols <- names(data()[, sapply(data(), is.numeric), drop = FALSE])
      updateCheckboxGroupInput(session, "corrgram_vars", 
                               selected = numeric_cols)
    })
    
    # Deselect All button functionality
    observeEvent(input$deselect_all_corrgram, {
      updateCheckboxGroupInput(session, "corrgram_vars", 
                               selected = character(0))
    })
    
    ### --- Hierarchy_Chart
    output$hierarchy_chart <- renderText({
      "Not yet implemented. Come back later."
    })
    
    
### DISTRIBUTIONS ###
    
    ### --- Boxplot
    
    output$boxplot <- renderPlot({
      req(data())
      plot_data <- data()
      
      # Filter for numeric columns
      numeric_cols <- sapply(plot_data, is.numeric)
      plot_data <- plot_data[, numeric_cols, drop = FALSE]
      plot_data <- switch(input$boxplot_portion,
                          "all" = plot_data,
                          "first_half" = plot_data %>% select(1:floor(ncol(.)/2)),
                          "second_half" = plot_data %>% select((floor(ncol(.)/2) + 1):ncol(.))
      )
      
      plot_data <- scale(plot_data, center = input$boxplot_center, scale = input$boxplot_scale)
      
      n_colors <- ncol(plot_data)
      color_palette <- colorRampPalette(brewer.pal(min(9, n_colors), "RdBu"))(n_colors)
      
      # Dynamic Title
      boxplot_title <- paste0("Boxplot (", 
                                   if(input$boxplot_center && input$boxplot_scale) {
                                     "Centered, Scaled"
                                   } else if(input$boxplot_center) {
                                     "Centered, Not Scaled"
                                   } else if(input$boxplot_scale) {
                                     "Not Centered, Scaled"
                                   } else {
                                     "Not Centered, Not Scaled"
                                   },
                                   if (input$boxplot_outliers) {
                                     paste0(", IQR Multiplier:", input$boxplot_iqr) 
                                   }," )")
      
      # Create the boxplot
      car::Boxplot(
        y = plot_data, 
        ylab = NA, 
        use.cols = TRUE, 
        notch = FALSE, 
        varwidth = FALSE,  
        horizontal = FALSE, 
        outline = input$boxplot_outliers, 
        col = color_palette,
        range = input$boxplot_iqr, 
        main = boxplot_title, 
        id = if (input$boxplot_outliers) list(n = 3, location = "avoid") else FALSE,
        las = 2,  # Rotate x-axis labels vertically
        cex.axis = 1.15  # Adjust text size if needed
      )
    })
    
    ### --- Histogram
    output$histogram <- renderText({
      "Not yet implemented. Come back later."
    })
      
### RELATIONSHIPS ###
    
    ### --- Time Series Plot
    
    get_numeric_cols <- function(data) {
      return(names(data)[sapply(data, is.numeric)])
    }
    
    # Function to get DateTime columns
    get_datetime_cols <- function(data) {
      return(names(data)[sapply(data, function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")))])
    }
    
    # Update choices for dependent variable
    observe({
      req(data())
      numeric_cols <- get_numeric_cols(data())
      updateSelectInput(session, "ts_dependent_var", 
                        choices = numeric_cols,
                        selected = numeric_cols[1])
    })
    
    # Update choices for DateTime variable
    observe({
      req(data())
      datetime_cols <- get_datetime_cols(data())
      updateSelectInput(session, "ts_datetime_var", 
                        choices = datetime_cols,
                        selected = datetime_cols[1])
    })
    
    # Reactive value to store current plot state
    current_plot <- reactiveVal("single")
    
    output$timeseries_plot <- renderPlot({
      req(input$relationshipsTabset == "Time Series Plot")
      req(data(), input$ts_datetime_var)
      
      plot_data <- data()
      datetime_var <- input$ts_datetime_var
      
      if (current_plot() == "single") {
        req(input$ts_dependent_var)
        # Ensure the selected variables exist in the data
        req(input$ts_dependent_var %in% names(plot_data),
            datetime_var %in% names(plot_data))
        
        # Create the single variable plot
        ggplot(plot_data, aes_string(x = datetime_var, y = input$ts_dependent_var)) +
          geom_line() +
          theme_minimal(base_size = 14) +  # Increase base font size
          labs(title = paste("Time Series Plot:", input$ts_dependent_var, "over time"),
               x = "Time",
               y = "Value") +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Larger, centered title
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold")
          )
      } else {
        # MultiPlot: Create a plot with all numeric variables on the same axis
        numeric_cols <- get_numeric_cols(plot_data)
        
        # Melt the data for ggplot
        library(reshape2)
        melted_data <- melt(plot_data, id.vars = datetime_var, measure.vars = numeric_cols)
        
        # Create the multi-line plot
        ggplot(melted_data, aes_string(x = datetime_var, y = "value", color = "variable")) +
          geom_line() +
          theme_minimal(base_size = 14) +  # Increase base font size
          labs(title = "Time Series Plot: All Numeric Variables",
               x = "Time",
               y = "Value") +
          theme(
            plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  # Larger, centered title
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = "right"
          ) +
          scale_color_discrete(name = "Variables")
      }
    }, height = 600, width = 800)  # Increase plot size
    
    # Observer for AutoPlot button
    observeEvent(input$autoplot_ts, {
      current_plot("auto")
    })
    
    # Observer for Reset button
    observeEvent(input$reset_ts, {
      current_plot("single")
    })
    
    ### --- Pairs Plot
    
    # Filter appropriate columns
    filter_columns <- function(data) {
      filtered_cols <- names(data)[sapply(data, function(col) {
        !(is.factor(col) && length(levels(col)) > 6)  # Exclude factors with more than 6 levels
         # !grepl("ID", names(col), ignore.case = TRUE)  # Exclude columns with "ID" in the name
      })]
      return(filtered_cols)
    }
    
    # Function to get categorical variables
    get_categorical_vars <- function(data) {
      cat_vars <- names(data)[sapply(data, function(col) {
        is.factor(col) || is.character(col) || (is.numeric(col) && length(unique(col)) <= 10)
      })]
      return(cat_vars)
    }
    
    # Update checkbox group and color-by select input when data changes
    observe({
      req(data())
      filtered_cols <- filter_columns(data())
      cat_vars <- get_categorical_vars(data())
      
      updateCheckboxGroupInput(session, "pairs_vars", 
                               choices = filtered_cols,
                               selected = character(0))  # Initially, no variables are selected
      
      updateSelectInput(session, "pairs_colour_by",
                        choices = c("None" = "", cat_vars),
                        selected = "")
    })
    
    # Update checkbox group when data changes
    observe({
      req(data())
      filtered_cols <- filter_columns(data())
      updateCheckboxGroupInput(session, "pairs_vars", 
                               choices = filtered_cols,
                               selected = character(0))  # Initially, no variables are selected
    })
    
    # Select All button functionality
    observeEvent(input$select_all_pairs, {
      filtered_cols <- filter_columns(data())
      updateCheckboxGroupInput(session, "pairs_vars", 
                               selected = filtered_cols)
    })
    
    # Deselect All button functionality
    observeEvent(input$deselect_all_pairs, {
      updateCheckboxGroupInput(session, "pairs_vars", 
                               selected = character(0))
    })
    
    # Randomly select 5 variables
    observeEvent(input$random_select_pairs, {
      req(data())
      filtered_cols <- filter_columns(data())
      if(length(filtered_cols) > 5) {
        selected_cols <- sample(filtered_cols, 5)
      } else {
        selected_cols <- filtered_cols
      }
      updateCheckboxGroupInput(session, "pairs_vars", 
                               selected = selected_cols)
    })
    
    output$pairs_plot <- renderPlot({
      # Only execute when the button is clicked
      req(input$pairs_generate)
      
      # Isolate the data retrieval to prevent unnecessary reactivity
      isolate({
        req(data())
        data <- data()
        
        # Check if any variables are selected
        req(length(input$pairs_vars) > 0, cancelOutput = TRUE)
        
        # Filter the data based on selected variables
        data_filtered <- data[, input$pairs_vars, drop = FALSE]
        
        tryCatch({
          # Create dynamic title
          plot_title <- if (input$pairs_colour_by != "") {
            paste0("Pairs Plot (Coloured by ", input$pairs_colour_by, ")")
          } else {
            "Pairs Plot"
          }
          
          # Base theme modifications
          base_theme <- theme(
            plot.title = element_text(size = input$pairs_text_size * 1.5, face = "bold", hjust = 0.5),  # Center and enlarge title
            axis.text = element_text(size = input$pairs_text_size),
            axis.title = element_text(size = input$pairs_text_size),
            legend.text = element_text(size = input$pairs_text_size),
            legend.title = element_text(size = input$pairs_text_size),
            strip.text = element_text(size = input$pairs_text_size)
          )
          
          if (input$pairs_colour_by != "") {
            # If a color variable is selected, include it in the ggpairs function
            p <- GGally::ggpairs(
              data = data,
              columns = input$pairs_vars,
              mapping = aes_string(color = input$pairs_colour_by,
                                   alpha = 0.5),
              title = plot_title,
              upper = list(continuous = wrap("cor", size = input$pairs_inner_text_size))
            ) + 
              base_theme +
              theme(legend.position = "bottom")
          } else {
            # If no color variable is selected, create the plot without color
            p <- GGally::ggpairs(
              data = data_filtered,
              mapping = aes_string(alpha = 0.5),
              title = plot_title,
              upper = list(continuous = wrap("cor", size = input$pairs_inner_text_size))
            ) +
              base_theme
          }
          
          # Adjust text size for plot labels
          p <- p + theme(text = element_text(size = input$pairs_text_size))
          
          p
        }, error = function(e) {
          # Return a blank plot with an error message
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = input$pairs_text_size / 3) +
            theme_void()
        })
      })
    })
    
    ### --- Mosaic Plot
    
    # Get suitable categorical variables
    get_suitable_vars <- function(data) {
      categorial_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
      suitable_vars <- categorial_vars[sapply(data[categorial_vars], function(x) length(unique(x)) <= 10)]
      return(suitable_vars)
    }
    
    # Update choices for mosaic plot variables
    observe({
      req(data())
      suitable_vars <- get_suitable_vars(data())
      
      updateSelectizeInput(session, "mosaic_variables", 
                           choices = suitable_vars,
                           selected = sample(suitable_vars, min(3, length(suitable_vars))))
    })
    
    # Render Mosaic Plot
    output$mosaic_plot <- renderPlot({
      req(data(), input$mosaic_variables, length(input$mosaic_variables) >= 2)
      
      tryCatch({
        formula <- as.formula(paste("~", paste(input$mosaic_variables, collapse = " + ")))
        vcd::mosaic(formula, data = data(),
                    split_vertical = input$mosaic_split_vertical,
                    #gp=shading_max,
                    main = "Mosaic Plot", 
                    shade = TRUE, 
                    #gp = shading_max, (Only works with 2 variables???)
                    #gp_args = list(level = c(0.2, 0.4)), # Adjust levels of shading_max
                    legend = TRUE)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, "Unable to create Mosaic Plot with selected variables", cex = 1.2)
        showNotification(paste("Error:", e$message), type = "error")
      })
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
                     p("It provides various tools for data visualization, summary statistics, and exploratory data analysis, and was inspired by an assignment I had in (DATA423: Data Science in Industry) at the University of Canterbury, New Zealand."),
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
                                tags$li("Apply optional cleaning of the data by uploading a .clean file."),
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
                     p("Special thanks to the University of Canterbury and the DATA423 course instructors"),
                     p("Some code (particularly for the visualisations) was modified from code provided in the course. (Authors: Nick Ward, Phil Davies, Nicki Cartlidge)")
                 ),
                 div(class = "content-section",
                     h3("Technologies Used"),
                     tags$ul(
                       tags$li("R + RShiny"),
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
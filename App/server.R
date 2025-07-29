
##########################################################################################################
#### SERVER SECTION - changed to work with write_list.R
##########################################################################################################

## TO-DO ####

## FIX: order_pattern()

# ShinyFile selection - decide how to select project path, method files, and output path
  # Vision: 
      # place to type the parent E:/ Project path, but you can edit. 
          # replaces Field 6, "path"
      # place to type the output parent path for the sequence file, but you can edit
      # place to type the folder path with all 12 methods, but you can edit

# Sample size adjustments 
# Continue openspecimen 



############### NOTES ABOUT SAMPLE LOGIC BY TYPE

#' total_count: rows that are not NA in Sample_ID** FIX
#' cal_curves: rows that start with 'FBS'
#' cc_count: number of rows that start with 'FBS'
#' MDL handling: code removes duplicate cal curve from cal_curves and assigns duplicate to mdls
#' pools: rows that start with 'HRE'
#' pool_count: number of rows of pools
#' pools1: pools that start with HRE and have .p1
#' pools2: pools that start with HRE and have .p2
#'  # requires pool_count to be 20! fix this?

#' waters: rows that start with 'Water'
#' water_count: number of rows of waters
#' amaps: rows that start with 'AM'
#' amap_count: number of rows of amaps
#' nists: rows that start with 'NIST'
#' nist_count: number of rows of nists
#' 
#' ## STUDY SAMPLES LOGIC #################
#' identify with an A or a K followed by 7 numbers in final_data$Sample_ID
#' 
#' total_samples: sum of cc_count + water_count + amap_count + nist_count + study_sample_count
#' 
#' ^ check against total_count, mismatches

### MSMS LOGIC
# if study_samples$pfms == TRUE then there is a PFAS MSMS run after that sample
# if study_samples$nonpfms == TRUE then there is a nonPFAS MSMS run after that sample

# for study samples: 
# batch_size: HARD-CODED TO 40

server <- function(input, output, session) {
  
  # Function to read CSV files with dynamic header detection
  read_csv_helper <- function(file_path) {
    
    raw_lines <- readLines(file_path, n = 30)  
    
    header_pattern <- "Tube Position|Tube ID|Rack ID"
    header_line <- grep(header_pattern, raw_lines)
    
    if (length(header_line) > 0) {
      skip_rows <- header_line[1] - 1  # Subtract 1 because we want to keep the header line
    } else {
      # Fallback: look for the first non-empty line
      non_empty_lines <- which(nzchar(trimws(raw_lines)))
      if (length(non_empty_lines) > 0) {
        skip_rows <- non_empty_lines[1] - 1
      } else {
        # Default if no clear header or non-empty line is found
        skip_rows <- 0
        warning("Could not detect header row, using first row as header")
      }
    }
    
    # Read the CSV with the determined number of rows to skip
    data <- read_csv(file_path,
                     skip = skip_rows,
                     col_names = TRUE,
                     trim_ws = TRUE,
                     show_col_types = FALSE)
    
    return(d = data)
  } # END CSV READER FUNCTION
  
  ###########################################################################################
  
  id_container <- reactiveVal(NULL)  # container to store the df as we go along
  debug_messages <- reactiveVal("")  # For debugging
  
  # Combined reactive values for all tabs
  values <- reactiveValues(
    samples_fetched = FALSE,
    run_info = list(
      date = NULL,
      study = NULL,
      batch = NULL,
      rack = NULL,
      tech = NULL,
      machine = NULL,
      position = NULL,
      path = NULL,
      firstlast = NULL
      )
  )
  

  #Folder paths for selected locations - 7/16
  selected_paths <- reactiveValues(
    project = NULL,
    method = NULL,
    output = NULL
  )
  
  
  #### PART ONE: MICRONIC PLATE SCAN DATA #########################################################################################
  
  # Dynamic UI for plate type selectors
  output$plate1_type_ui <- renderUI({
    req(input$plate1)
    div(
      style = "margin-bottom: 15px;",
      selectInput("plate1_type", "Plate 1 Type:", 
                  choices = c("Study_Sample", "QAQC"),
                  selected = "Study_Sample")
    )
  })
  
  output$plate2_type_ui <- renderUI({
    req(input$plate2)
    div(
      style = "margin-bottom: 15px;",
      selectInput("plate2_type", "Plate 2 Type:", 
                  choices = c("Study_Sample", "QAQC"),
                  selected = "Study_Sample")
    )
  })
  
  output$plate3_type_ui <- renderUI({
    req(input$plate3)
    div(
      style = "margin-bottom: 15px;",
      selectInput("plate3_type", "Plate 3 Type:", 
                  choices = c("Study_Sample", "QAQC"),
                  selected = "QAQC") # Default to QAQC for plate 3
    )
  })
  
  output$plate4_type_ui <- renderUI({
    req(input$plate4)
    div(
      style = "margin-bottom: 15px;",
      selectInput("plate4_type", "Plate 4 Type:", 
                  choices = c("Study_Sample", "QAQC"),
                  selected = "QAQC") # Default to QAQC for plate 4
    )
  })
  
  
  ################# INPUT CSV LOGIC ###################
  observeEvent(input$process, {
    # Safety check - at least one file must be provided
    if(is.null(input$plate1) && is.null(input$plate2) && is.null(input$plate3) && is.null(input$plate4)) {
      debug_messages(paste(debug_messages(), "Error: Please upload at least one plate CSV file.", sep="\n"))
      return()
    }
    
    # Collect file inputs in a list
    file_inputs <- list(
      input$plate1,
      input$plate2,
      input$plate3,
      input$plate4
    )
    
    # Collect plate type inputs in a list
    plate_types <- list(
      if (!is.null(input$plate1)) input$plate1_type else "Null",
      if (!is.null(input$plate2)) input$plate2_type else "Null",
      if (!is.null(input$plate3)) input$plate3_type else "Null",
      if (!is.null(input$plate4)) input$plate4_type else "Null"
    )
    
    debug_messages(paste(debug_messages(), "Processing files...", sep="\n"))
    
    #### RACK 1 #################################
    plate1_input <- file_inputs[[1]]
    if (is.null(plate1_input)) {
      debug_messages(paste(debug_messages(), "No file for Rack 1, creating placeholder", sep="\n"))
      # Create a placeholder data frame that matches the structure of a rack with data
      d <- data.frame(
        ID = character(96),      
        pos96 = character(96),   
        num384 = numeric(96),   
        pos384 = character(96),
        sampletype = character(96)
      )
      d$pos96 <- map$pos96[1:96]
      d$num384 <- map$num384[1:96]
      d$pos384 <- map$pos384[1:96]
      d$sampletype <- plate_types[[1]]
      
      rack1 <- d #Final rack1 if no input
      
      
    } else {
      debug_messages(paste(debug_messages(), "Processing Rack 1 file", sep="\n"))
      
      # If csv file exists, use the Tube Ids and connect to 384-position
      file_path <- plate1_input$datapath
      d <- read_csv_helper(file_path) #helper function at top
      
      d <- d[-97, ] #remove footer
      d <- d %>% select('Tube Position', 'Tube ID')
      d <- d %>% mutate(well = paste0(df$Well, "-", 1)) # Add well column for plate 1
      
      #^ Right order ; add number for tracking
      d$order96 <- 1:96
      
      # Find and add positions in 384-well plate (df 'map')
      rack1 <- merge(d, 
                     map[, c("pos96", "Position384", "num384", "pos384")],  
                     by.x = "well",                         
                     by.y = "pos96",                        
                     all.x = TRUE) %>%
        mutate(num384 = as.numeric(num384)) %>%
        arrange(order96) 
      
      rack1 <- rack1 %>% # Final rack1
        select(
          ID = `Tube ID`,
          pos96 = well,
          num384,
          pos384)%>%
        mutate(sampletype = plate_types[[1]])
    } #End rack1
    
    #### RACK 2 #################################
    plate2_input <- file_inputs[[2]]
    if (is.null(plate2_input)) {
      debug_messages(paste(debug_messages(), "No file for Rack 2, creating placeholder", sep="\n"))
      # Create a placeholder data frame that matches the structure of a rack with data
      d <- data.frame(
        ID = character(96),      
        pos96 = character(96),   
        num384 = numeric(96),   
        pos384 = character(96),
        sampletype = character(96)
      )
      d$pos96 <- map$pos96[97:192]
      d$num384 <- map$num384[97:192]
      d$pos384 <- map$pos384[97:192]
      d$sampletype <- plate_types[[2]]
      
      rack2 <- d # Final rack2 if no file input
      
    } else {
      debug_messages(paste(debug_messages(), "Processing Rack 2 file", sep="\n"))
      # If csv file exists, use the Tube Ids and connect to 384-position
      
      file_path <- plate2_input$datapath
      d <- read_csv_helper(file_path) #helper function at top
      
      d <- d[-97, ] #remove footer
      d <- d %>% select('Tube Position', 'Tube ID')
      d <- d %>% mutate(well = paste0(df$Well, "-", 2)) # Add well column for plate 2
      
      #^ Right order ; add number for tracking
      d$order96 <- 1:96
      
      # Find and add positions in 384-well plate (df 'map')
      rack2 <- merge(d, 
                     map[, c("pos96", "Position384", "num384", "pos384")],  
                     by.x = "well",                         
                     by.y = "pos96",                        
                     all.x = TRUE) %>%
        mutate(num384 = as.numeric(num384)) %>%
        arrange(order96) 
      
      rack2 <- rack2 %>% # Final rack2 if file input
        select(
          ID = `Tube ID`,
          pos96 = well,
          num384,
          pos384) %>%
        mutate(sampletype = plate_types[[2]])
    } #End rack2
    
    #### RACK 3 ##############################
    plate3_input <- file_inputs[[3]]
    if (is.null(plate3_input)) {
      debug_messages(paste(debug_messages(), "No file for Rack 3, creating placeholder", sep="\n"))
      # Create a placeholder data frame that matches the structure of a rack with data
      d <- data.frame(
        ID = character(96),      
        pos96 = character(96),   
        num384 = numeric(96),   
        pos384 = character(96),
        sampletype = character(96)
      )
      d$pos96 <- map$pos96[193:288]
      d$num384 <- map$num384[193:288]
      d$pos384 <- map$pos384[193:288]
      d$sampletype <- plate_types[[3]]
      
      rack3 <- d # Final rack3 if no file input
      
    } else {
      debug_messages(paste(debug_messages(), "Processing Rack 3 file", sep="\n"))
      # If csv file exists, use the Tube Ids and connect to 384-position
      
      file_path <- plate3_input$datapath
      d <- read_csv_helper(file_path) #helper function at top
      
      d <- d[-97, ] #remove footer
      d <- d %>% select('Tube Position', 'Tube ID')
      d <- d %>% mutate(well = paste0(df$Well, "-", 3)) # Add well column for plate 3
      
      #^ Right order ; add number for tracking
      d$order96 <- 1:96
      
      # Find and add positions in 384-well plate (df 'map')
      rack3 <- merge(d, 
                     map[, c("pos96", "Position384", "num384", "pos384")],  
                     by.x = "well",                         
                     by.y = "pos96",                        
                     all.x = TRUE) %>%
        mutate(num384 = as.numeric(num384)) %>%
        arrange(order96) 
      
      rack3 <- rack3 %>% # Final rack3 if file input
        select(
          ID = `Tube ID`,
          pos96 = well,
          num384,
          pos384) %>%
        mutate(sampletype = plate_types[[3]])
    } #End rack3
    
    #### RACK 4 ##################################
    plate4_input <- file_inputs[[4]]
    
    if (is.null(plate4_input)) {
      debug_messages(paste(debug_messages(), "No file for Rack 4, creating placeholder", sep="\n"))
      # Create a placeholder data frame that matches the structure of a rack with data
      d <- data.frame(
        ID = character(96),      
        pos96 = character(96),   
        num384 = numeric(96),   
        pos384 = character(96),
        sampletype = character(96)
      )
      d$pos96 <- map$pos96[289:384]
      d$num384 <- map$num384[289:384]
      d$pos384 <- map$pos384[289:384]
      d$sampletype <- plate_types[[4]]
      
      rack4 <- d # Final rack4 if no file input
      
    } else {
      debug_messages(paste(debug_messages(), "Processing Rack 4 file", sep="\n"))
      # If csv file exists, use the Tube Ids and connect to 384-position
      file_path <- plate4_input$datapath
      d <- read_csv_helper(file_path) #helper function at top
      
      d <- d[-97, ] #remove footer
      d <- d %>% select('Tube Position', 'Tube ID')
      d <- d %>% mutate(well = paste0(df$Well, "-", 4)) # Add well column for plate 4
      
      #^ Right order ; add number for tracking
      d$order96 <- 1:96
      
      # Find and add positions in 384-well plate (df 'map')
      rack4 <- merge(d, 
                     map[, c("pos96", "Position384", "num384", "pos384")],  
                     by.x = "well",                         
                     by.y = "pos96",                        
                     all.x = TRUE) %>%
        mutate(num384 = as.numeric(num384)) %>%
        arrange(order96) 
      
      rack4 <- rack4 %>% # Final rack4 if file input
        select(
          ID = `Tube ID`,
          pos96 = well,
          num384,
          pos384) %>%
        mutate(sampletype = plate_types[[4]])
    } #End rack4
    
    ####### COMBINE RACKS #############################################
    tryCatch({
      debug_messages(paste(debug_messages(), "Combining racks...", sep="\n"))
      rack_list <- list(rack1, rack2, rack3, rack4)
      all_racks <- do.call(rbind, rack_list) #384 obs. of 4 var
      # convert position to integer
      all_racks$num384 <- as.integer(all_racks$num384)
      # convert ID to character
      all_racks$ID <- as.character(all_racks$ID)
      # ADD ROW AND COLUMN TRACKING COLUMNS
      all_racks$Col_order_by_plate <- map$col_order_by_plate
      all_racks$Row_order_by_plate <- map$row_order_by_plate
      # Remove white space - critical
      all_racks$ID <- gsub("\\s+", "", all_racks$ID)
      
      debug_messages(paste(debug_messages(), "Successfully combined racks.", sep="\n"))
      id_container(all_racks)  # store in reactive container
      
      # Switch to TAB 2 after processing
      shinyjs::delay(1000, {
        updateTabsetPanel(session, "tabs", selected = "2. Check Layout")
      })
      
      
    }, error = function(e) {
      debug_messages(paste(debug_messages(), paste("Error combining racks:", e$message), sep="\n"))
    }) # end try catch
    
  }) ############# END INPUT CSV LOGIC ##################################
  
  # Debug output
  output$debug_output <- renderText({
    debug_messages()
  })
  
  # Matrix ID results from Plate Scans
  output$result_table1 <- renderTable({
    req(id_container())
    display_data <- id_container()
    
    # Hide columns
    display_data$Col_order_by_plate <- NULL
    display_data$Row_order_by_plate <- NULL
    display_data$num384 <- NULL
    
    # Rename columns
    names(display_data)[names(display_data) == "pos96"] <- "96-wp position-Plate"
    names(display_data)[names(display_data) == "pos384"] <- "384-wp position"
    names(display_data)[names(display_data) == "sampletype"] <- "Sample Type"
    
    # Return the filtered and ordered data
    display_data
  })
  
  
  # Visualization of the 96-well plate
  
  # Helper function to create a single plate plot
  create_plate_plot <- function(data, plate_num) {
    # Filter data for this specific plate
    # Extract the plate number from pos96 (format like "A1-1")
    plate_data <- data %>%
      filter(grepl(paste0("-", plate_num, "$"), pos96))
    
    plate_type <- plate_data$sampletype[1]
    
    plate_data <- plate_data%>% 
      filter(ID != "") # if ID exists, keep. otherwise remove
    
    # Create a base plot with a grid
    rows <- LETTERS[1:8]
    cols <- 1:12
    
    # Create empty plot
    plot <- ggplot() +
      # Grid for all wells
      geom_tile(data = expand.grid(row = 1:8, col = 1:12),
                aes(x = col, y = row),
                fill = "white", color = "#333333", linewidth = 0.8) +
      # Set axis labels
      scale_y_reverse(breaks = 1:8, labels = rows) +
      scale_x_continuous(breaks = 1:12) +
      labs(
        title = paste("Plate", plate_num, "-", plate_type),
        x = NULL,  # Remove x-axis title
        y = NULL   # Remove y-axis title
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    
    # If we have data for this plate, highlight the filled wells
    if(nrow(plate_data) > 0) {
      # Extract row and column from pos96
      filled_wells <- plate_data %>%
        mutate(
          well = sub("-\\d+$", "", pos96),
          row = match(substr(well, 1, 1), rows),
          col = as.numeric(substr(well, 2, nchar(well)))
        )
      
      if (plate_type == "Study_Sample") {
      # Add colored points for filled wells
      plot <- plot +
        geom_point(data = filled_wells, 
                   aes(x = col, y = row),
                   size = 3, color = "#0D98BA")
      }
      
      if (plate_type == "QAQC") {
        # Add colored points for filled wells
        plot <- plot +
          geom_point(data = filled_wells, 
                     aes(x = col, y = row),
                     size = 3, color = "#71797E")
      } 
    
    } # end if nrow plate data > 0 statement
    
    return(plot)
  }
  
  # Create the four plate outputs
  output$plate1 <- renderPlot({
    req(id_container())
    create_plate_plot(id_container(), 1)
  })
  
  output$plate2 <- renderPlot({
    req(id_container())
    create_plate_plot(id_container(), 2)
  })
  
  output$plate3 <- renderPlot({
    req(id_container())
    create_plate_plot(id_container(), 3)
  })
  
  output$plate4 <- renderPlot({
    req(id_container())
    create_plate_plot(id_container(), 4)
  })
  
  ################# END TAB 2 ########################
  
  #### TAB 3: SAMPLE TYPES #########################################################################################
  
  values <- reactiveValues(
    samples_fetched = FALSE #do not show until ready
  )
  
  # MATCH SAMPLES LOGIC #########################
  # This is the Fetch sample ID button.
  observeEvent(input$match_samples, {
    req(id_container())
    req(exists("qaqc_inventory")) 
    req(exists("sample_inventory")) 
    
    # Get current data
    current_data <- id_container() 
    
    # Add Sample_ID column if it doesn't exist
    if (!"Sample_ID" %in% names(current_data)) {
      current_data$Sample_ID <- NA_character_
    }

    # Create vectors to track positions for ordering analysis
    matched_ids <- c()
    matched_positions <- c()
    
    # Process each row according to its sampletype #######################
    for (i in 1:nrow(current_data)) {
      matrix_id <- current_data$ID[i] #get matrix ID (ex: K0876323, NA for empty or QAQC)
      
      # First - skip empty IDs
      if (is.na(matrix_id) || matrix_id == "") {
        current_data$Sample_ID[i] <- NA 
        next
      }
      
      # Get the sampletype
      sample_type <- current_data$sampletype[i]
      
      if (sample_type == "QAQC") {
        match_row <- which(qaqc_inventory$Matrix_ID == matrix_id) 
        if (length(match_row) > 0 ) { 
          current_data$Sample_ID[i] <- qaqc_inventory$Sample_ID[match_row[1]] #in case repeat copy?
          # Store for ordering analysis
          matched_ids <- c(matched_ids, matrix_id)
          matched_positions <- c(matched_positions, current_data$pos384[i])
          
        } else {
          # If no match found, leave empty
          current_data$Sample_ID[i] <- ""
        }
        
      } else if (sample_type == "Study_Sample") {
        match_row <- which(sample_inventory$Matrix_ID == matrix_id) 
        if (length(match_row) > 0) {
          current_data$Sample_ID[i] <- sample_inventory$Sample_ID[match_row[1]] #in case repeat copy
          # Store for ordering analysis
          matched_ids <- c(matched_ids, matrix_id)
          matched_positions <- c(matched_positions, current_data$pos384[i])
          
        } else {
          # If no match found, leave empty
          current_data$Sample_ID[i] <- ""
        }
      }
    } # END LOOP THROUGH MATRIX ID CHECK! 
    
    # Update id_container with the modified data!!
    id_container(current_data)
    
    ### HELPFUL INFORMATION ####
    # If the row is supposed to be empty (no tube), Sample_ID will be NA
    # If no sample ID was found in inventory, Sample_ID will be ""
    
    #### Are samples ordered by row or column? ########################################################################################
    order_pattern <- reactiveVal("bycol")  # Default value
    
    if (length(matched_positions) >= 4) {
      # Extract row letters and column numbers
      rows <- substr(matched_positions, 1, 1)
      cols <- as.numeric(substr(matched_positions, 2, nchar(matched_positions)))
      
      # Check pattern of changes
      row_changes <- sum(diff(match(rows, LETTERS)) != 0, na.rm=TRUE)
      col_changes <- sum(diff(cols) != 0, na.rm=TRUE)
      
      # Determine ordering pattern and notify
      if (row_changes > col_changes) {
        order_pattern("byrow") 
      } else {
        order_pattern("bycol")
      }
    }
    
    # 7/21: add reactive message 
    output$order_message <- renderUI({
      order_text <- switch(order_pattern(),
                           "bycol" = "column of each plate.",
                           "byrow" = "row of each plate.",
                           "an unknown pattern")
      
      tags$div(
        style = "font-size: 16px; font-weight: 500; color: #333;",
        paste("The sample inventory appears to be organized by", order_text)
      )
    })
    
    ########################################################################################################################################

    # Show success message
    showNotification("Sample ID matching completed", type = "message")
    values$samples_fetched <- TRUE
  }) ##### Match function complete
  
  #count matches - UI for right column ##################
  output$matchstatus <- renderUI({
    req(id_container())
    data <- id_container()
    
    if ("Sample_ID" %in% names(data)) {
      # For Sample type
      sample_rows <- data %>% 
        filter(sampletype == "Study_Sample" & !is.na(ID) & ID != "")
      sample_matched <- sample_rows %>% 
        filter(!is.na(Sample_ID) & Sample_ID != "")
      sample_ratio <- paste0(nrow(sample_matched), " / ", nrow(sample_rows))
      
      # For QAQC type
      qaqc_rows <- data %>% 
        filter(sampletype == "QAQC" & !is.na(ID) & ID != "")
      qaqc_matched <- qaqc_rows %>% 
        filter(!is.na(Sample_ID) & Sample_ID != "")
      qaqc_ratio <- paste0(nrow(qaqc_matched), " / ", nrow(qaqc_rows))
      
      # Calculate totals
      total_matched <- nrow(sample_matched) + nrow(qaqc_matched)
      total_rows <- nrow(sample_rows) + nrow(qaqc_rows)
      
      # Create UI output
      tagList(
        p(strong("Match Summary:")),
        div(
          style = "display: flex; flex-direction: column; gap: 5px;",
          p(style = "margin: 0;", strong("Samples matched:"), " ", sample_ratio),
          p(style = "margin: 0;", strong("QAQCs matched:"), " ", qaqc_ratio),
          p(style = "margin: 0; border-top: 1px solid #ddd; padding-top: 5px;", 
            strong("Total:"), " ", paste0(total_matched, " / ", total_rows))
        ),
        br(),
        div(
          style = "display: flex; gap: 10px; margin-top: 10px;",
          actionButton("save_to_env", "Save df to R", class = "btn-success"),
          actionButton("gototab4", "Next tab", class = "btn-primary", 
                       style = "font-size: 16px; padding: 8px 20px;")
        )
      )
    } else {
      p(em("Click 'Match IDs to inventory' to see statistics"))
    }
  })
  

  # Display and store ordered data based on user choice
  ordered_data <- reactive({
    req(id_container())
    req(input$order_by)
    
    display_data <- id_container()
    
    order_button <- input$order_by
    if (!is.null(order_button) && order_button %in% names(display_data)) {
      display_data <- display_data[order(display_data[[order_button]]), ]
    }
    
    # Remove unnecessary columns
    display_data$Col_order_by_plate <- NULL
    display_data$Row_order_by_plate <- NULL
    display_data$num384 <- NULL
    
    # Rename columns
    names(display_data)[names(display_data) == "pos96"] <- "96-wp position-Plate"
    names(display_data)[names(display_data) == "pos384"] <- "384-wp position"
    names(display_data)[names(display_data) == "sampletype"] <- "Sample Type"
    
    display_data
  })
  
  output$result_table2 <- renderTable({
    ordered_data()
  })
  
  # QAQC PLATE LABELLING ####################################################
  # Create a UI output that conditionally renders the plot, fixed 5/13
  output$qaqc_plate_container <- renderUI({
    # Check if the value exists and is TRUE
    if(!is.null(values$samples_fetched) && values$samples_fetched) {
      plotOutput("qaqc_plate", height = "500px")
    } else {
      div(
        style = "height: 500px; display: flex; align-items: center; justify-content: center; background-color: #f9f9f9; border: 1px dashed #ccc;",
        p("QAQC plate will appear here after clicking 'Match IDs to inventory'")
      )
    }
  })
  
  ### QAQC LABELING PLOT ########################
  output$qaqc_plate <- renderPlot({
    req(id_container())
    req(values$samples_fetched)
    
    # First, find which plate(s) contain QAQC samples
    qaqc_data <- id_container() %>%
      filter(!is.na(ID) & ID != "" & sampletype == "QAQC") # has ID and is QAQC
    
    # If no QAQC samples, return message plot
    if(nrow(qaqc_data) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No QAQC samples found", size = 6) +
          theme_void()
      )
    }
    
    # Extract the last digit from pos96 (format like "A1-3")
    plate_num <- as.numeric(sub(".*-(\\d+)$", "\\1", qaqc_data$pos96[1]))
    
    # Filter data for this specific plate with QAQC samples only
    plate_data <- id_container() %>%
      filter(grepl(paste0("-", plate_num, "$"), pos96)) %>%
      filter(!is.na(ID) & ID != "" & sampletype == "QAQC")
    
    
    # Create a base plot with a grid
    rows <- LETTERS[1:8]
    cols <- 1:12
    
    # Create plot
    plot <- ggplot() +
      # Add darker and thicker grid for all wells
      geom_tile(data = expand.grid(row = 1:8, col = 1:12),
                aes(x = col, y = row),
                fill = "white", color = "#333333", size = 0.8) +
      # Set axis labels
      scale_y_reverse(breaks = 1:8, labels = rows) +
      scale_x_continuous(breaks = 1:12) +
      coord_cartesian(xlim = c(0.5, 12.5), ylim = c(9.5, 0.5), expand = TRUE) +
      labs(
        title = paste("QAQC Samples - Plate", plate_num),
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.text = element_text(size = 9),
        plot.margin = margin(30, 30, 60, 30)
      )
    
    grouped_data <- plate_data %>%
      mutate(
        # Group samples together
        sample_group = case_when(
          grepl("^FBS", Sample_ID) ~ "Cal_Curve", #change this to FBS LATER
          grepl("^HRE", Sample_ID) ~ "HRE_Pool",
          grepl("^Water", Sample_ID) ~ "Water",
          grepl("^AM", Sample_ID) ~ "AMAP",
          grepl("^NIST", Sample_ID) ~ "NIST",
          TRUE ~ substr(Sample_ID, 1, 3) # Fallback to first 3 letters for others
        )
      )
    
    # Extract row and column from pos96 and prepare for plotting
    filled_wells <- grouped_data %>%
      mutate(
        well = sub("-\\d+$", "", pos96),
        row = match(substr(well, 1, 1), rows),
        col = as.numeric(substr(well, 2, nchar(well)))
      )
    
    # Create a color palette for the sample groups
    group_colors <- c(
      "Cal_Curve" = "#E41A1C",  # Red
      "HRE_Pool" = "#377EB8",   # Blue
      "Water" = "#4DAF4A",      # Green
      "AMAP" = "#FF7F00",       # Orange
      "NIST" = "#984EA3"        # Purple
    )
    
    # Plot QAQC samples their sample_group colors
    plot <- plot +
      geom_point(data = filled_wells, 
                 aes(x = col, y = row, color = sample_group),
                 size = 5)+
      scale_color_manual(values = group_colors,
                         name = "QAQC Type")
    return(plot)
  })
  
  ####################################################################################################################################
  # Check and add matrix IDs from inventory ###################################
  
  # Path to the Excel files assigned in run_app.R changed 7/21
  q_path <- file.path("..", q_filepath)
  s_path <- file.path("..", s_filepath)

  observeEvent(input$add_matrix_ids, {
    # Show processing message
    showNotification("Checking for Matrix ID matches...", 
                     type = "message", duration = NULL, id = "checking_notification")

    
    # Check if inventory files exist
    if (!file.exists(q_path)) {
      removeNotification(id = "checking_notification")
      showNotification(paste("Error: File not found at", q_path), 
                       type = "error", duration = 10)
      return()
    }
    if (!file.exists(s_path)) {
      removeNotification(id = "checking_notification")
      showNotification(paste("Error: File not found at", s_path), 
                       type = "error", duration = 10)
      return()
    }
    
    tryCatch({
      
      # Get the current Matrix IDs that have been processed
      current_ids <- id_container()$ID
      
      # Read just the necessary sheets with only required columns
      qaqcs_data <- read_excel(q_path, sheet = "QAQCs")
      qaqc_matches <- sum(qaqcs_data$Matrix_ID %in% current_ids)
      
      # Count QAQCs that match and are already analyzed
      qaqc_already_analyzed <- sum(qaqcs_data$Matrix_ID %in% current_ids & qaqcs_data$Analyzed == TRUE, na.rm = TRUE)
      
      # Read 'Samples' sheet to find matches
      samples_data <- read_excel(s_path, sheet = "Samples")
      sample_matches <- sum(samples_data$Matrix_ID %in% current_ids)
      
      # Count Samples that match and are already analyzed
      sample_already_analyzed <- sum(samples_data$Matrix_ID %in% current_ids & samples_data$Analyzed == TRUE, na.rm = TRUE)
      
      # Remove checking notification
      removeNotification(id = "checking_notification")
      
      # Determine if we need to show a warning
      show_warning <- (qaqc_already_analyzed > 0 || sample_already_analyzed > 0)
      
      # Create a modal dialog with appropriate message
      if (show_warning) {
        showModal(modalDialog(
          title = "WARNING: Already Analyzed Items Found",
          div(
            p(sprintf("WARNING: File indicates %d QAQCs and %d Samples have already been analyzed. Are you sure you want to continue?", 
                      qaqc_already_analyzed, sample_already_analyzed)),
            p(sprintf("Total matches found: %d QAQCs and %d Samples.", 
                      qaqc_matches, sample_matches))
          ),
          footer = tagList(
            actionButton("cancel_matrix_update", "Cancel", class = "btn-default"),
            actionButton("confirm_matrix_update", "Continue Anyway", class = "btn-warning")
          ),
          easyClose = FALSE
        ))
      } else {
        showModal(modalDialog(
          title = "Matrix ID Matches Found",
          div(
            p(sprintf("Found %d QAQC matches and %d Sample matches that will be marked as 'Analyzed'.", 
                      qaqc_matches, sample_matches)),
            p("Do you want to continue with updating the Excel file?")
          ),
          footer = tagList(
            actionButton("cancel_matrix_update", "Cancel"),
            actionButton("confirm_matrix_update", "Update Excel File", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      }
      
      # Store match information for use in the NEXT STEP --------------
      values$excel_update_info <- list(
        q_path = q_path,
        s_path = s_path,
        qaqcs_data = qaqcs_data,
        samples_data = samples_data,
        current_ids = current_ids
      )
      
    }, error = function(e) {
      # Handle errors
      removeNotification(id = "checking_notification")
      showNotification(paste("Error checking Excel file:", e$message), 
                       type = "error", duration = 10)
    })
  })#### END the check and popup box for updating excel file! ####################################
  
  # Cancel handler
  observeEvent(input$cancel_matrix_update, {
    removeModal()
    showNotification("Excel update cancelled", type = "message", duration = 3)
  })
  
  ##### Confirmation handler 
  ##### ACTUALLY UPDATE ######################################################
  observeEvent(input$confirm_matrix_update, {
    removeModal()
    showNotification("Updating Excel file...", type = "message", 
                     duration = NULL, id = "updating_notification")
    
    tryCatch({
      # Retrieve stored info
      info <- values$excel_update_info
      
      ### QAQC UPDATE ####################################
      # Load the workbook with write privileges
      wb <- openxlsx::loadWorkbook(info$q_path)
      
      # Update 'QAQCs' workbook
      for(i in 1:nrow(qaqc_inventory)) { #for all rows in the QAQC data
        if(qaqc_inventory$Matrix_ID[i] %in% info$current_ids) {
          qaqc_inventory$Analyzed[i] <- TRUE
        }
      }
      openxlsx::writeData(wb, sheet = "QAQCs", x = qaqc_inventory, colNames = TRUE)
      
      # Save the workbook
      openxlsx::saveWorkbook(wb, info$q_path, overwrite = TRUE)
      
      ### SAMPLE UPDATE ####################################
      # Load the workbook with write privileges
      wb2 <- openxlsx::loadWorkbook(info$s_path)
      
      # Update 'Samples' workbook
      for(i in 1:nrow(sample_inventory)) {
        if(sample_inventory$Matrix_ID[i] %in% info$current_ids) {
          sample_inventory$Analyzed[i] <- TRUE
        }
      }
      openxlsx::writeData(wb2, sheet = "Samples", x = sample_inventory, colNames = TRUE)
      
      # Save the workbook
      openxlsx::saveWorkbook(wb2, info$s_path, overwrite = TRUE)
      
      removeNotification(id = "updating_notification")
      showNotification("Matrix IDs successfully marked as analyzed in the Excel files!", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      # Handle errors
      removeNotification(id = "updating_notification")
      showNotification(paste("Error updating Excel file:", e$message), 
                       type = "error", duration = 10)
    })
  }) ### END CONFIRM - UPDATE MATRIX ID 'ANALYZED' Column.
  
  
  ####################################################################################################################################
  
  # 'Next button'
  # Observer for the gototab4 button
  observeEvent(input$gototab4, {
    # Switch to the fourth tab when the button is clicked
    updateTabsetPanel(session, "tabs", selected = "4. Generate sequence list")
  })
  
  
  
  # Tab 4- generate sequence list 
  ######################################################################################################

  # Run info UI with form fields
  output$run_info <- renderUI({
    req(id_container())
    
    div(
      style = "padding: 15px; background-color: #e8f4f8; border: 1px solid #d1e0e0; border-radius: 5px;",
      h4("Run Information", style = "margin-top: 0; margin-bottom: 15px; color: #2c3e50;"),
      
      fluidRow(
        column(6,
               # Text input fields
               textInput("date_input", "Date", 
                         placeholder = "e.g., 230415"),
               
               textInput("study_input", "Study)",
                         placeholder = "e.g., CLU0120_MEC"),
               
               textInput("batch_input", "Batch",
                         placeholder = "e.g., 01"),
               
               textInput("rack_input", "Study Sample Rack Numbers (separated by comma)",
                         placeholder = "e.g., 7,8"),
               
               textInput("technician_input", "Technician", 
                         placeholder = "e.g., CM")
               
        ),
        
        column(6,
               
               # Select fields
               selectInput("instrument_input", "Instrument",
                           choices = c("C18", "HILIC")),
               
               selectInput("position_input", "Position",
                           choices = c("R", "B", "G", "Y", "CH01", "CH02", "CH03", "CH04", "CH05", "CH06", "CH07", "CH08", "CH09")),
               
               radioButtons("first_last_batch", "First or last batch? (still in development)",
                            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                            selected = FALSE,
                            inline = TRUE) # make this actually do something?
        )
      ),
      
      # Add save button
      div(
        style = "margin-top: 15px; text-align: right;",
        actionButton("save_run_info", "Update Run Info", 
                     class = "btn-primary", 
                     style = "background-color: #3498db;")
      )
    )
  })
  
  # Observer for saving run info
  observeEvent(input$save_run_info, {
    # Validate inputs
    req(input$date_input, input$study_input, input$batch_input, input$rack_input, 
        input$technician_input, input$instrument_input, input$position_input, 
        input$first_last_batch ) 
    
    # Update reactive values
    values$run_info$date <- input$date_input
    values$run_info$study <- input$study_input
    values$run_info$batch <- input$batch_input
    values$run_info$rack <- input$rack_input
    values$run_info$tech <- input$technician_input
    values$run_info$machine <- input$instrument_input
    values$run_info$position <- input$position_input
    values$run_info$firstlast <- as.logical(input$first_last_batch)
    
    # Show confirmation to user
    showNotification("Run information updated.", 
                     type = "message", 
                     duration = 3)
  })
  
  ## Add user-selected folder paths ##########################################################################
  ## Working 7/22 ? 

  # Specify roots 
  roots <- c(
    "Home" = normalizePath("~"),
    "Root" = "C:/",         # or "/" on Linux/Mac
    "R Drive" = "R:/"
  )
  
  # Enable folder selection for each input
  shinyDirChoose(input, 'project_path', roots = roots, session = session) # Project path - auto?
  shinyDirChoose(input, 'method_path', roots = roots, session = session) # Method files - select folder then auto-assign method?
  shinyDirChoose(input, 'output_path', roots = roots, session = session) # Output path - type?
  
  # Observe selections and convert to full paths
  observe({
    if (is.list(input$project_path) && !is.null(input$project_path$path)) {
      selected_paths$project <- parseDirPath(roots, input$project_path)
      cat("Selected project path:", selected_paths$project, "\n")
    }
    
    if (is.list(input$method_path) && !is.null(input$method_path$path)) {
      selected_paths$method <- parseDirPath(roots, input$method_path)
      cat("Selected method path:", selected_paths$method, "\n")
    }
    
    if (is.list(input$output_path) && !is.null(input$output_path$path)) {
      selected_paths$output <- parseDirPath(roots, input$output_path)
      cat("Selected output path:", selected_paths$output, "\n")
    }
  })
  
  # GENERATE SEQUENCE LIST -------------------------------------------------------------------------
  # UI - button
  output$generate_list <- renderUI({
    req(id_container())
    
    div(
      style = "padding: 10px; background-color: #e8f4f8; border: 1px solid #d1e0e0; border-radius: 5px;",
      actionButton("write_list", "Generate Sequence List", class = "btn-primary"),
      br(), br(),
      # Add section for status 
      uiOutput("sequence_status")
    )
  })
  
  # Functions for later #####################################################################
  
  # UI for sequence status - fixed 7/17
  output$sequence_status <- renderUI({
    output_path <- selected_paths$output
    
    # Only proceed if a folder has been selected and it's a valid character string
    if (!is.null(output_path) &&
        is.character(output_path) &&
        nzchar(output_path) &&
        dir.exists(output_path)) {
      
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #e8f8e8; border: 1px solid #d0e0d0; border-radius: 5px;",
        h4("Sequence List Generated!", style = "margin-top: 0; color: #2c3e50;"),
        p(paste("Folder selected:", basename(output_path))),
        div(
          style = "display: flex; gap: 10px;",
          actionButton("view_sequence", "View Folder", icon = icon("eye"), 
                       style = "color: white; background-color: #3498db;")
        )
      )
      
    } else {
      # Return nothing until the folder is selected
      NULL
    }
  })
  
  # Save to environment button
  observeEvent(input$save_to_env, {
    req(id_container())
    
    # Get the current data
    data_to_save <- id_container()
    
    # Save to the global environment
    assign("final_plate_data", data_to_save, envir = .GlobalEnv)
    # Show a success notification
    showNotification(
      "Data saved to R environment as 'final_plate_data'", 
      type = "message",
      duration = 5
    )
  })
  
  # View button handler
  observeEvent(input$view_sequence, {
    req(values$generated_file_path)
    
    if (.Platform$OS.type == "windows") {
      shell.exec(values$generated_file_path)
    } else if (.Platform$OS.type == "unix") {
      system2("open", values$generated_file_path)
    } else {
      showNotification("Cannot open file automatically on this system. Please locate the file manually.", 
                       type = "warning", duration = 5)
    }
  })
  
  output$debug_console <- renderText({
    debug_messages()
  })
  
  # Add a clear button handler
  observeEvent(input$clear_debug, {
    # Reset the debug messages
    debug_messages("")
  })
  
  # Capture debug information throughout the process
  debug_log <- function(msg) {
    # Print to R console
    cat("[DEBUG]", msg, "\n")
    # Also update the Shiny debug messages if they exist
    if(exists("debug_messages") && is.reactive(debug_messages)) {
      current_msgs <- debug_messages()
      debug_messages(paste(current_msgs, msg, sep="\n"))
    }
  }
  
  ### Function to get a unique filename (for saving, so it's not overwritten)
  get_unique_filename <- function(filepath) {
    original_path <- filepath
    counter <- 1
    
    # Extract the file extension and base path
    file_ext <- tools::file_ext(filepath)
    if (file_ext != "") {
      base_path <- sub(paste0("\\.", file_ext, "$"), "", filepath)
      file_ext <- paste0(".", file_ext)
    } else {
      base_path <- filepath
      file_ext <- ""
    }
    
    # Check if file exists and increment counter until we find a unique name
    while (file.exists(filepath)) {
      debug_log(paste("File already exists:", filepath))
      filepath <- paste0(base_path, " (", counter, ")", file_ext)
      counter <- counter + 1
      debug_log(paste("Trying new filename:", filepath))
    }
    
    # If the filename changed, log it
    if (filepath != original_path) {
      debug_log(paste("Original filename already exists. Using new filename:", filepath))
    }
    
    return(filepath)
  }
  
  
####################################################################################################################################################
# Observer for generating sequence list ######################################################################################################################
  observeEvent(input$write_list, {
    
    debug_log("1. Generate sequence list button clicked")
    
    # Check the reactiveValues structure itself
    debug_log(paste("2. Values exist:", !is.null(values)))
    
    # Safer checking of required inputs - one at a time
    debug_log("3. Checking required inputs...")
    if (is.null(values$run_info)) {
      debug_log("ERROR: values$run_info is NULL")
      showNotification("Missing run information", type = "error")
      return()
    }
    
    debug_log(paste("4. Date:", ifelse(is.null(values$run_info$date), "NULL", values$run_info$date)))
    debug_log(paste("5. Study:", ifelse(is.null(values$run_info$study), "NULL", values$run_info$study)))
    debug_log(paste("6. Batch:", ifelse(is.null(values$run_info$batch), "NULL", values$run_info$batch)))
    debug_log(paste("6a. Racks:", ifelse(is.null(values$run_info$rack), "NULL", values$run_info$rack)))
    debug_log(paste("7. Tech:", ifelse(is.null(values$run_info$tech), "NULL", values$run_info$tech)))
    debug_log(paste("8. Machine:", ifelse(is.null(values$run_info$machine), "NULL", values$run_info$machine)))
    debug_log(paste("9. Position:", ifelse(is.null(values$run_info$position), "NULL", values$run_info$position)))
    debug_log(paste("First/Last:", ifelse(is.null(values$run_info$firstlast), "NULL", values$run_info$firstlast)))

    # 7/21 add folder paths ?
    # 7/21 - put safeguards so you can't mess up the format?
    
    
    # Check if any required values are missing
    missing_values <- character(0)
    if (is.null(values$run_info$date)) missing_values <- c(missing_values, "date")
    if (is.null(values$run_info$study)) missing_values <- c(missing_values, "study")
    if (is.null(values$run_info$batch)) missing_values <- c(missing_values, "batch")
    if (is.null(values$run_info$rack)) missing_values <- c(missing_values, "rack")
    if (is.null(values$run_info$tech)) missing_values <- c(missing_values, "tech")
    if (is.null(values$run_info$machine)) missing_values <- c(missing_values, "machine")
    if (is.null(values$run_info$position)) missing_values <- c(missing_values, "position")
    if (is.null(values$run_info$firstlast)) missing_values <- c(missing_values, "firstlast")

    if (length(missing_values) > 0) {
      debug_log(paste("ERROR: Missing required values:", paste(missing_values, collapse=", ")))
      showNotification(paste("Missing required values:", paste(missing_values, collapse=", ")), type = "error")
      return()
    }
    
    debug_log("12. All required run info values exist")
    
    # Access the run info values 
    date_value <- values$run_info$date
    study_value <- values$run_info$study
    batch_value <- values$run_info$batch
    rack_value <- values$run_info$rack
    tech_value <- values$run_info$tech
    machine_value <- values$run_info$machine
    position_value <- values$run_info$position
    firstlast_value <- values$run_info$firstlast
    
    # Folder paths
    project_path <- selected_paths$project
    method_path <- selected_paths$method
    output_path <- selected_paths$output
    
    # Create filename with inputs
    filename_value <- paste0(sub("_.*$", "", study_value),"_", batch_value,"_", date_value,"_", machine_value)
    
    debug_log("Successfully accessed all run info values")
    
    ## Added 7/21 - update QAQCS.xlsx and Samples.xlsx with the study,batch,date ################
    debug_log("13. Updating QAQCS.xlsx with study info...")

    current_ids <- id_container()$ID #does this work?
    # Update QAQCS #### FIXED 7/29 - did not work if you did not update sheet earlier (info reactive Value)
    tryCatch({
      wb <- openxlsx::loadWorkbook(q_path)
    }, error = function(e) {
      stop("Failed to load workbook. It might be open in Excel or locked.")
    })
    
    for(i in 1:nrow(qaqc_inventory)) {
      if(qaqc_inventory$Matrix_ID[i] %in% current_ids) { #get current ids some other way
        #Update with study, batch #, date
        qaqc_inventory$Study_Batch_YYMMDD[i] <- paste0(sub("_.*$", "", study_value),"_", batch_value,"_", date_value)
      }
    }
    openxlsx::writeData(wb, sheet = "QAQCs", x = qaqc_inventory, colNames = TRUE)
    
    # Save the workbook
    openxlsx::saveWorkbook(wb, q_path, overwrite = TRUE)
    
    debug_log("Successfully updated QAQCS.xlsx")
    
    # Update Samples ####
    tryCatch({
      wb2 <- openxlsx::loadWorkbook(s_path)
    }, error = function(e) {
      stop("Failed to load workbook. It might be open in Excel or locked.")
    })
    
    for(i in 1:nrow(sample_inventory)) {
      if(sample_inventory$Matrix_ID[i] %in% current_ids) { # check?
        #Update with study, batch #, date
        sample_inventory$Study_Batch_YYMMDD[i] <- paste0(sub("_.*$", "", study_value),"_", batch_value,"_", date_value)
      }
    }
    openxlsx::writeData(wb2, sheet = "Samples", x = sample_inventory, colNames = TRUE)
    
    # Save the workbook
    openxlsx::saveWorkbook(wb2, s_path, overwrite = TRUE)
    
    debug_log("Successfully updated Samples.xlsx")
  
    #######################################################################
    
    # Check id_container()
    debug_log("14. Checking sample data...")
    if (!exists("id_container")) {
      debug_log("ERROR: id_container function does not exist")
      showNotification("Internal error: id_container is missing", type = "error")
      return()
    }
    
    # Get the sample data cautiously
    tryCatch({
      final_data <- id_container()
      debug_log("15. Successfully retrieved data from id_container()")
      
      if(is.null(final_data)) {
        debug_log("ERROR: id_container() returned NULL")
        showNotification("Sample data is NULL", type = "error")
        return()
      }
      
      if(!is.data.frame(final_data)) {
        debug_log(paste("ERROR: id_container() returned non-data.frame:", class(final_data)))
        showNotification("Sample data is not a data frame", type = "error")
        return()
      }
      
      if(nrow(final_data) == 0) {
        debug_log("ERROR: Sample data is empty (0 rows)")
        showNotification("Sample data is empty", type = "error")
        return()
      }
      
      debug_log(paste("16. Sample data contains", nrow(final_data), "rows and", ncol(final_data), "columns"))
      debug_log(paste("17. Sample data columns:", paste(names(final_data), collapse=", ")))
      
    }, error = function(e) {
      debug_log(paste("ERROR in id_container():", e$message))
      showNotification(paste("Error accessing sample data:", e$message), type = "error")
      return()
    })
    
    # Verify the working directory
    tryCatch({
      app_wd <- getwd()
      debug_log(paste("18. Working directory:", app_wd))
      
      # # Go up one directory - comment out 5/29
      # parent_wd <- dirname(app_wd)
      # debug_log(paste("18a. Adjusted to parent directory:", parent_wd))
      
      # Check for required script files with more detail
      script_file <- "write_list.R"
      missing_files <- character(0)
      
      # changed 5/29, putting write_list into App folder
      file_path <- file.path(app_wd, script_file)
      
      if(!file.exists(file_path)) {
        missing_files <- c(missing_files, script_file)
        debug_log(paste("WARNING: Required file not found:", file_path))
      } else {
        file_size <- file.info(file_path)$size
        debug_log(paste("19. Found file:", script_file, "- Size:", file_size, "bytes"))
      }
      
      if(length(missing_files) > 0) {
        error_msg <- paste("Missing required files:", paste(missing_files, collapse=", "))
        debug_log(error_msg)
        showNotification(error_msg, type = "error", duration = 10)
        return()
      }
      
      debug_log("20. All required script files exist")
      
    }, error = function(e) {
      debug_log(paste("ERROR checking working directory:", e$message))
      showNotification(paste("Error checking files:", e$message), type = "error")
      return()
    })
    
    # Ensure filename has .csv extension
    if(!grepl("\\.csv$", filename_value)) {
      filename_value <- paste0(filename_value, ".csv")
    }
    
    debug_log(paste("21. Output filename:", filename_value))
    
    # Set output file path ##########################################################################
    # CHANGED 5/29 to instrument folder
    # Changed 7/16 to selected_paths$output (from new input button)
    
    if(machine_value == "C18") {
      # old - hardcode destination
      directory <- "R:/diwalke/1-RAW Files/230908-RawFiles_LC-Exploris120-RALPH/CLU0120_250501_MEC_C18/3-Sequence_Files"
      
      #directory <- selected_paths$output # not gonna happen
      sequence_filepath <- file.path(directory, filename_value)
      debug_log(paste("22. Original full output path:", sequence_filepath))
     
    } else if(machine_value == "HILIC") {
      # old - hardcode destination
      # directory <- "R:/diwalke/1-RAW Files/230908-RawFiles_LC-Exploris120-NANCY/CLU0120_250501_MEC_HILIC/3-Sequence_Files"
      
      directory <- selected_paths$output #already a character path
      sequence_filepath <- file.path(directory, filename_value)
      debug_log(paste("22. Original full output path:", sequence_filepath))
    }
      
    # Check if file exists and get unique filename
    sequence_filepath <- get_unique_filename(sequence_filepath)
    
    # Update filename_value to match the new path (for display purposes)
    filename_value <- basename(sequence_filepath)
    debug_log(paste("22a. Unique output path:", sequence_filepath))
    
    # Test file writing with more detail
    tryCatch({
      # Check file write permissions
      output_dir <- dirname(sequence_filepath)
      debug_log(paste("23. Double-check output directory:", output_dir))
      
      if(!dir.exists(output_dir)) {
        debug_log(paste("WARNING: Output directory doesn't exist:", output_dir))
        create_result <- try(dir.create(output_dir, recursive = TRUE))
        if(inherits(create_result, "try-error")) {
          debug_log(paste("ERROR: Failed to create output directory:", geterrmessage()))
          showNotification("Cannot create output directory", type = "error")
          return()
        }
      }
      
      # Test if we can write to the directory
      test_file <- file.path(output_dir, ".write_test")
      write_test <- try(write.csv(data.frame(test=1), test_file))
      if(inherits(write_test, "try-error")) {
        debug_log(paste("ERROR: Cannot write to output directory:", geterrmessage()))
        showNotification("Cannot write to output directory", type = "error")
        return()
      } else {
        file.remove(test_file)
        debug_log("24. Successfully tested write permissions")
      }
      
    }, error = function(e) {
      debug_log(paste("ERROR testing file permissions:", e$message))
      showNotification(paste("Error with file permissions:", e$message), type = "error")
      return()
    })
    
    # Create a progress indicator with more early updates
    debug_log("25. Starting progress indicator")
    
    withProgress(message = 'Generating sequence list', value = 0, {
      # Update progress immediately to show something is happening
      incProgress(0.1, detail = "Initializing...")
      
      # Use a simplified direct approach first to identify issues
      debug_log("26. Starting main sequence generation in try-catch block")
      
      tryCatch({
        debug_log("27. Setting global variables...")
        
        # Assign values to the global environment
        assign("date", date_value, envir = .GlobalEnv)
        assign("study", study_value, envir = .GlobalEnv)
        assign("batch", batch_value, envir = .GlobalEnv)
        assign("rack", rack_value, envir = .GlobalEnv)
        assign("tech", tech_value, envir = .GlobalEnv)
        assign("machine", machine_value, envir = .GlobalEnv)
        assign("pos", position_value, envir = .GlobalEnv)
        assign("firstlast", firstlast_value, envir = .GlobalEnv)
        
        assign("final_data", final_data, envir = .GlobalEnv)
        
        assign("sequence_filename", filename_value, envir = .GlobalEnv)
        assign("sequence_filepath", sequence_filepath, envir = .GlobalEnv)
        
        assign("project_folder", project_path, envir = .GlobalEnv)
        assign("method_folder", method_path, envir = .GlobalEnv)
        assign("output_folder", output_path, envir = .GlobalEnv)
        # 7/21 do something with these in write_list?
        
        
        
        debug_log("28. All variables set in global environment")
        
        app_wd <- getwd()
        
        # Proceed to machine-specific script
        incProgress(0.5, detail = paste("Processing", machine_value, "sequence..."))
        
        if(machine_value == "C18") {
          debug_log("29. Attempting to source write_list.R to generate C18 sequence list...")
          
          source_result <- try({
            source(file.path(app_wd, "write_list.R"))
          }, silent = FALSE)
          
          if(inherits(source_result, "try-error")) {
            debug_log(paste("ERROR in write_list.R:", geterrmessage()))
            stop(paste("Error in write_list.R:", geterrmessage()))
          }
          
          debug_log("30. Successfully executed write_list.R")
        } else {
          debug_log("29. Attempting to source write_list.R to generate HILIC sequence list...")
          
          source_result <- try({
            source(file.path(app_wd, "write_list.R"))
          }, silent = FALSE)
          
          if(inherits(source_result, "try-error")) {
            debug_log(paste("ERROR in write_list.R:", geterrmessage()))
            stop(paste("Error in write_list.R:", geterrmessage()))
          }
          
          debug_log("30. Successfully executed write_list.R")
        }
        
        # Verify output file exists
        incProgress(0.9, detail = "Verifying output...")
        
        if(file.exists(sequence_filepath)) {
          debug_log(paste("31. SUCCESS: File created at", sequence_filepath))
          debug_log(paste("32. File size:", file.info(sequence_filepath)$size, "bytes"))
          
          # Store the path for UI updates
          values$generated_file_path <- sequence_filepath
          
          # Show success notification
          showNotification(
            paste0(
              "Sequence list successfully generated! ",
              "Saved as: ", filename_value
            ), 
            type = "message", 
            duration = 5
          )
        } else {
          debug_log(paste("ERROR: Output file was not created at", sequence_filepath))
          
          # List what files were created in the directory
          files_created <- list.files(output_dir, pattern = "\\.csv$")
          if(length(files_created) > 0) {
            debug_log(paste("CSV files in directory:", paste(files_created, collapse=", ")))
          }
          
          showNotification("Sequence file was not created", type = "error")
        }
        
      }, error = function(e) {
        error_msg <- paste("ERROR:", e$message)
        debug_log(error_msg)
        showNotification(error_msg, type = "error", duration = 10)
      }, finally = {
        # Clean up global environment
        debug_log("33. Cleaning up global variables...")
        cleanup_vars <- c("date", "study", "batch", "rack", "tech", "machine", "pos", 
                          "firstlast", "sequence_filename", "final_data", "sequence_filepath")
        rm(list = intersect(cleanup_vars, ls(.GlobalEnv)), envir = .GlobalEnv)
      })
      
      # Complete progress
      debug_log("34. Updating progress indicator to complete")
      incProgress(1, detail = "Complete!")
    })
    
    debug_log("35. Sequence generation process complete")
  }) #################### END 'WRITE LIST' OBSERVER ############################################################################################
} # END SERVER 
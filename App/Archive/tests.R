# testing


  
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
load("Mapping/map.Rdata")  # loads 'df' (96-positions and orders) and 'map' (384-positions)

###########################################################################################
# MANUALLY SET PATHS FOR TESTING 
file_inputs <- list("R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0120_Batch22_Rack43_20250710_093449.csv",
                    "R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0120_Batch22_Rack44_20250710_093538.csv", 
                    "R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0120_Batch22_QAQC_20250710_093734.csv", 
                    NULL)

plate_types <- list("Study_Sample", "Study_Sample", "QAQC", "NULL")

#### RACK 1 #################################
plate1_input <- file_inputs[[1]]
if (is.null(plate1_input)) {
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
  # If csv file exists, use the Tube Ids and connect to 384-position
  file_path <- file_inputs[[1]]
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
  
  file_path <- file_inputs[[2]]
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
  
  file_path <- file_inputs[[3]]
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
  file_path <- file_inputs[[4]]
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

# all_racks combined - 384 obs of 7 variables

###########################################################################################################################################################

## MATCH LOGIC 

q_filepath <- "QAQCS.xlsx"
s_filepath <- "Samples.xlsx"

qaqcs <- read_excel(q_filepath, sheet = "QAQCs")
samples <- read_excel(s_filepath, sheet = "Samples")

# as-is
qaqc_inventory <- qaqcs
sample_inventory <- samples

#############################################################
current_data <- all_racks
  
# Add Sample_ID column - will be filled in from inventory
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
    # For QAQC samples, look up the Sample_ID in qaqcs dataframe
    match_row <- which(qaqc_inventory$Matrix_ID == matrix_id) #INVENTORY
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
    # For study samples, Sample_ID and Matrix_ID in inventory are the same
    match_row <- which(sample_inventory$Matrix_ID == matrix_id) #INVENTORY
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

# If the row is supposed to be empty (no tube), Sample_ID will be NA
# If no sample ID was found in inventory, Sample_ID will be ""






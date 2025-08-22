# testing
# 8/21 ####################


final_data <- final_plate_data

order_pattern <- "byrow"
order_pattern <- "bycol"

order_column <- if (order_pattern == "byrow") {
  "Row_order_by_plate"
} else if (order_pattern == "bycol")  {
  "Col_order_by_plate"
}

order_column

ordered_study_samples <- final_data[order(final_data[[order_column]]), ]



if (machine == "C18") {
  suggested_method_folder <- r'(C:\Xcalibur\methods\Aria_C18_Methods)'
  name <- 'RALPH'
} else if (machine == "HILIC") {
  suggested_method_folder <- r'(C:\Xcalibur\methods\Aria Methods)'
  name <- 'NANCY'
}

method_folder <- r'(C:\Xcalibur\methods\Aria_C18_Methods)'
method_folder <- "C:/Xcalibur/methods/Aria_C18_Methods" #this works


method_folder <- "C:/Xcalibur/methods/Aria Methods"

# List all files in folder 
all_files <- list.files(method_folder, full.names = TRUE)
# all_files is empty.

# REGULAR
reg_pos <- all_files[grepl("120k", all_files, ignore.case = TRUE) &
                       grepl("Bottom", all_files, ignore.case = TRUE)]
reg_neg <- all_files[grepl("120k", all_files, ignore.case = TRUE) &
                       grepl("Top", all_files, ignore.case = TRUE)]
# MS2 PFAS
ms2_pfas_pos <- all_files[grepl("PFAS-inc", all_files, ignore.case = TRUE) &
                            grepl("Bottom", all_files, ignore.case = TRUE)]
ms2_pfas_neg <- all_files[grepl("PFAS-inc", all_files, ignore.case = TRUE) &
                            grepl("Top", all_files, ignore.case = TRUE)]
# MS2 NON PFAS
ms2_nonpfas_pos <- all_files[grepl("+PP", all_files, ignore.case = TRUE) &
                               grepl("Bottom", all_files, ignore.case = TRUE)]
ms2_nonpfas_neg <- all_files[grepl("+PP", all_files, ignore.case = TRUE) &
                               grepl("Top", all_files, ignore.case = TRUE)]
# MS2 NON PFAS
ms2_pool_pos <- all_files[grepl("Water-ddMS2", all_files, ignore.case = TRUE) &
                            grepl("Bottom", all_files, ignore.case = TRUE)]
ms2_pool_neg <- all_files[grepl("Water-ddMS2", all_files, ignore.case = TRUE) &
                            grepl("Top", all_files, ignore.case = TRUE)]



############################################################################

### FIRST CHUNKS - DATA Join #######

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
# List of required packages
required_packages <- c(
  "openxlsx", "shinyjs", "tools",
  "shiny", "readr", "dplyr", 
  "plotly", "readxl", "shinyFiles", "stringr", "this.path"
)

# Install any that are missing
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Set the wd to the folder the app is in - MORE ROBUST with this.path changed 7/29
setwd(this.dir())

load("Mapping/map.Rdata")  # loads 'df' (96-positions and orders) and 'map' (384-positions)

###########################################################################################
# MANUALLY SET PATHS FOR TESTING 


file_inputs <- list("R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0007-2_Batch3_Rack6_20250625_141058.csv",
                    "R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0007-2_Batch4_Rack7_20250627_125608.csv", 
                    "R:/diwalke/LC/Run_Lists/Plate_Scans/CLU0007-2_Batch3_QAQC_20250625_141149.csv", 
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
current_data <- all_racks # NO sample_ID column yet.

# inventory_loc
# [1] 726   6  86 166 246 326 406 486 566 646 736  16  96 176 256 336 416 496 576 656 746  26 106 186 266 346 426 506 586 666 756  36 116 196 276
# [36] 356 436 516 596 676 766  46 126 206 286 366 446 526 606 686 776  56 136 216 296 376 456 536 616 696 786  66 146 226 306 386 466 546 626 706
# [71] 796  76 156 236 316 396 476 556 636 716 727   7  87 167 247 327 407 487 567 647 737  17  97 177 257 337 417 497 577 657 747  27 107 187 267
# [106] 347 427 507 587 667 757  37 117 197 277 357 437 517 597 677 767  47 127 207 287 367 447 527 607 687 777  57 137 217 297 377 457 537 617 697
# [141] 787  67 147 227 307 387 467 547 627 707 797  77 157 237 317 397 477 557 637 717 807 808


#experiment with changed order: 
current_data <- current_data[order(current_data$Row_order_by_plate), ]

# > inventory_loc
# [1] 726 566 416 266 116 766 606 456 306 156   6 646 496 346 196  46 686 536 386 236  86 736 576 426 276 126 776 616 466 316 166  16 656 506 356
# [36] 206  56 696 546 396 246  96 746 586 436 286 136 786 626 476 326 176  26 666 516 366 216  66 706 556 406 256 106 756 596 446 296 146 796 636
# [71] 486 336 186  36 676 526 376 226  76 716 727 567 417 267 117 767 607 457 307 157 807   7 647 497 347 197  47 687 537 387 237 808  87 737 577
# [106] 427 277 127 777 617 467 317 167  17 657 507 357 207  57 697 547 397 247  97 747 587 437 287 137 787 627 477 327 177  27 667 517 367 217  67
# [141] 707 557 407 257 107 757 597 447 297 147 797 637 487 337 187  37 677 527 377 227  77 717

# Add Sample_ID column if it doesn't exist
if (!"Sample_ID" %in% names(current_data)) {
  current_data$Sample_ID <- NA_character_
}

#Track positions for ordering analysis
inventory_loc <- c()

# Process each row according to its sampletype #######################
for (i in 1:nrow(current_data)) {
  matrix_id <- current_data$ID[i] #get matrix ID (ex: K0876323)
  
  # First - skip empty IDs
  if (is.na(matrix_id) || matrix_id == "") {
    current_data$Sample_ID[i] <- NA # IF MATRIX ID originally NA, make sample ID NA.
    next
  }
  
  # Get the sampletype
  sample_type <- current_data$sampletype[i]
  
  if (sample_type == "QAQC") {
    match_row <- which(qaqc_inventory$Matrix_ID == matrix_id) 
    if (length(match_row) > 0 ) { 
      current_data$Sample_ID[i] <- qaqc_inventory$Sample_ID[match_row[1]] #in case repeat?
    }
      
  } else if (sample_type == "Study_Sample") {
    match_row <- which(sample_inventory$Matrix_ID == matrix_id) # where in the inventory does it match?
    
    if (length(match_row) > 0) {
      current_data$Sample_ID[i] <- sample_inventory$Matrix_ID[match_row[1]] # collect MATRIX ID. changed 8/12
      
      # Store location for ordering analysis
      inventory_loc <- c(inventory_loc, match_row)
      
    } else {
      # If no match found, leave empty
      current_data$Sample_ID[i] <- ""
    }
  }
} # END LOOP THROUGH MATRIX ID CHECK! # good

inventory_loc #check

  diffs <- diff(inventory_loc)
  
  # LOGIC: Check pattern of changes
  # inventory_loc is currenty same order as current_data; plate 1 a1, a2, a3.. a12, b1 ... (row by row)
  # if the numbers in inventory_loc more or less go up one at a time, it's organized by row
  # if they skip around, it's organized by column
  
  evidence_for_row_by_row <- sum(diffs == 1)
  evidence_for_col_by_col <- sum(diffs == 12)
  
  detected <- if (evidence_for_row_by_row > 2 * evidence_for_col_by_col) {
    "byrow"
  } else if (evidence_for_col_by_col > 2 * evidence_for_row_by_row) {
    "bycol"
  } else {
    "uk"
  }
  
  # Default to byrow if unclear
  if (detected == "uk") {
    detected_order_pattern("byrow")
    cat("Sample order unclear, defaulting to by row. Change in app if desired\n")
  } else {
    detected_order_pattern(detected)


# 5/29 test for mdl 

load("Mapping/map.Rdata")

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

## RACK 1 ###########
file_path <- "R:/diwalke/LC/Run_Lists/Plate_Scans/RackDataT_CLU0120_Batch8_Rack15_20250529_103303.csv"
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
mutate(sampletype = 'Study_Sample')


#### RACK 2 #################################

file_path <- "R:/diwalke/LC/Run_Lists/Plate_Scans/RackDataT_CLU0120_Batch8_Rack16_20250529_103430.csv"
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
  mutate(sampletype = 'Study_Sample')


#### RACK 3 ##############################
file_path <- "R:/diwalke/LC/Run_Lists/Plate_Scans/RackDataT_CLU0120_Batch8_QAQC_20250529_103508.csv"
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
  mutate(sampletype = 'QAQC')



# RACK 4 - NULL ###########
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
d$sampletype <- 'NULL'
  
rack4 <- d # Final rack4 if no file input


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
all_racks$ID <- gsub("//s+", "", all_racks$ID)

## Match sample_ID #############################################

##### INVENTORY FIRST 

inventory_file_path <- "R:/diwalke/LC/Run_Lists/CLU0120 (MEC)/Sequence_Generator/TestCopy_250409_CLU0120_Plate_Loading_Order_Updated-Again.xlsx"

qaqcs <- read_excel(inventory_file_path, sheet = "QAQCs")
samples <- read_excel(inventory_file_path, sheet = "Samples")

# remove extra cols
qaqcs <- qaqcs %>% 
  select(-QAQC_RN, -Analyzed_ID) 

samples <- samples %>% 
  select(-Analyzed_ID) 

## JOIN QAQCS and SAMPLE DATA TO CLU0120_inventory
CLU0120_inventory <- bind_rows(qaqcs, samples) 

################# MATCH SAMPLE IDS ########################

current_data <- all_racks 

# Add Sample_ID column if it doesn't exist
if (!"Sample_ID" %in% names(current_data)) {
  current_data$Sample_ID <- NA_character_
}

# Add RackNum column if it doesn't exist
if (!"RackNum" %in% names(current_data)) {
  current_data$RackNum <- NA_character_  # Initialize the column
}

# Create vectors to track positions for ordering analysis
matched_ids <- c()
matched_positions <- c()

# Process each row according to its sampletype
for (i in 1:nrow(current_data)) {
  matrix_id <- current_data$ID[i]
  
  # Skip empty IDs
  if (is.na(matrix_id) || matrix_id == "") {
    current_data$RackNum[i] <- NA
    current_data$Sample_ID[i] <- NA 
    next
  }
  
  # Get the sampletype
  sample_type <- current_data$sampletype[i]
  
  # Handle differently based on sampletype
  if (sample_type == "QAQC") {
    # For QAQC samples, look up the Sample_ID in qaqcs dataframe
    match_row <- which(CLU0120_inventory$Matrix_ID == matrix_id)
    if (length(match_row) > 0 ) { 
      current_data$Sample_ID[i] <- CLU0120_inventory$Sample_ID[match_row[1]] #in case repeat copy?
      # Also collect Rack number
      current_data$RackNum[i] <- CLU0120_inventory$RackNum[match_row[1]]
      # Store for ordering analysis
      matched_ids <- c(matched_ids, matrix_id)
      matched_positions <- c(matched_positions, current_data$pos384[i])
      
    } else {
      # If no match found, leave empty
      current_data$Sample_ID[i] <- ""
    }
    
  } else if (sample_type == "Study_Sample") {
    # For study samples, Sample_ID and Matrix_ID in CLU0120_inventory are the same
    match_row <- which(CLU0120_inventory$Matrix_ID == matrix_id)
    if (length(match_row) > 0) {
      # POTENTIALLY USE THE OG STUDY INVENTORY INSTEAD!!
      current_data$Sample_ID[i] <- CLU0120_inventory$Sample_ID[match_row[1]] #in case repeat copy
      # Also collect Rack number
      current_data$RackNum[i] <- CLU0120_inventory$RackNum[match_row[1]]
      
      # Store for ordering analysis
      matched_ids <- c(matched_ids, matrix_id)
      matched_positions <- c(matched_positions, current_data$pos384[i])
      
    } else {
      # If no match found, leave empty
      current_data$Sample_ID[i] <- ""
    }
  }
} # END LOOP THROUGH MATRIX ID CHECK! 

#############################################################################
# Count by sample type - needed for loops
#############################################################################

final_data <- current_data 

final_data$Sample_ID <- trimws(final_data$Sample_ID) #remove spaces 
total_count <- sum(!is.na(final_data$Sample_ID)) #for checking
# 194 = 80 + 80 + 34 correct

cal_curves <- final_data[grepl("FBS", final_data$Sample_ID), ] 
cc_count <- nrow(cal_curves)

## 5/29 - find MDL by finding duplicate cal_curves 

dup_sample_ids <- cal_curves$Sample_ID[duplicated(cal_curves$Sample_ID)]
# "FBS_0.1ng-mL"
dup_rows <- cal_curves[cal_curves$Sample_ID %in% dup_sample_ids, ] # two rows that have that ID
# Keep the one with the lowest num384

dup_to_remove <- dup_rows[ave(dup_rows$Col_order_by_plate, dup_rows$Sample_ID, FUN = max) == dup_rows$Col_order_by_plate, ]

#Remove the duplicate from cal_curves
cal_curves <- cal_curves[!cal_curves$ID %in% dup_to_remove$ID, ] #yes

#assign duplicate to mdls
mdls <- dup_to_remove #yes

#############################################

pools <- final_data[grepl("^HRE", final_data$Sample_ID), ] 
pool_count <- nrow(pools)

waters <- final_data[grepl("^Water", final_data$Sample_ID), ]
water_count <- nrow(waters)
amaps <- final_data[grepl("^AM", final_data$Sample_ID), ]
amap_count <- nrow(amaps)
nists <- final_data[grepl("^NIST", final_data$Sample_ID), ]
nist_count <- nrow(nists)

pattern_a <- "^A\\d{7}$"
pattern_k <- "^K\\d{7}$"

study_samples <- final_data[grepl(pattern_a, final_data$Sample_ID) | 
                              grepl(pattern_k, final_data$Sample_ID), ]
study_sample_count <- nrow(study_samples)

  
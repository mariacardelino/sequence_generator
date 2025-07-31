################################################################################################
# Changing to new default (40 samples/chunk, 4 chunks). 
# Making adaptable to racks of more or less than 80 samples (expected).
################################################################################################

# 7/28 changed to new order:
# 1 real blank (front and back labeled 'Blank', middle labeled 'Cal_Std')
# 1 water (out of 2)
# 1 NIST (OPTIONAL, out of 1)

# Pool 1 
# Pool 2
# 3 PFAS (0.1, 0.5, 1)
# 40 study samples
# Pool 1
# Pool 2
# 40 study samples
# Pool 1
# Pool 2
# 3 PFAS (0.1, 0.5, 1)

# 1 'blank' 
# CAL CURVE (1-8/8)
# 1 'blank' 

# Pool 1
# Pool 2
# 40 study samples
# Pool 1
# Pool 2
# 40 study samples
# Pool 1
# Pool 2
# 3 PFAS (0.1, 0.5, 1)
# 1 AMAP (out of 1)
# 1 water (out of 2)
# 1 real blank

### HANDLING
# Expecting 80 samples per plate (2 plates)
# If 60+: split in half
# If <60: do only one chunk

### Passing arguments directly with function -- 7/31 change
write_sequence <- function(params) {
  # params <- list(
  # date = date_value,
  # study = study_value,
  # batch = batch_value,
  # rack = rack_value,
  # tech = tech_value,
  # machine = machine_value,
  # pos = position_value,
  # firstlast = firstlast_value,
  # final_data = final_data,
  # sequence_filename = filename_value,
  # sequence_filepath = sequence_filepath,
  # project_path = project_path,
  # method_folder = method_folder,
  # output_path = output_path,
  # ordering_preference = order_pattern()
  # )

  # TRIM WHITE SPACE 
  final_data$Sample_ID <- trimws(final_data$Sample_ID, which = "left")
  
  # Variables needed #######################################
  # Field 1
  sample_type <- 'Unknown' #always Unknown (for now?)
  
  # Field 2 - File Name
  # R or N?
  if (machine == 'C18') {
    letter <- 'R' #? Scylla/charybdis?
  } else if (machine == 'HILIC') {
    letter <- 'N'
  } 
  
  studynum <- sub("_.*$", "", study)
  
  filename_pos <- sprintf("%s%s_%s_%spos_", letter, date, studynum, machine)
  filename_neg <- sprintf("%s%s_%s_%sneg_", letter, date, studynum, machine)
  
  # Field 3 - Sample ID 
  # comes from dataframe by row
  
  # Field 4 - Comment 
  comment <- as.numeric(batch)
  
  # Field 5 - L2 Client
  l2 <- "add"
  
  # Field 6 - PROJECT Path - this will be selectable later on....
  ## 7/31 change to be edited by user in last tab
  
  #endpath <- sprintf("Batch_%s_%s", batch, date) #Creating the end of the project path (unique to study, batch, and date)
  #path <- sprintf('E:/Projects/%s_%s_%s/1-Raw_Files/%s/', study, date, machine, endpath) # ---------------------------------------------
  
  path <- project_path
  ms2path <- paste0(path, '/MSMS')
  
  # Field 7 - Instrument Method 
  
  ### UPDATED 7/31 - should work for both C18 and HILIC.
  
  # List all files in folder 
  all_files <- list.files(method_folder, full.names = TRUE)
  
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
  
  # Optional: grab the first match
  # target_file <- matched_file[1]
  # folder <- method_folder
  
  ##### OLD #####
  # C18
  # reg_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240408_C18pos_120k_BottomPump_Channel1"
  # reg_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240408_C18neg_120k_TopPump_Channel2"
  # 
  # ms2_pfas_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240802_C18pos_BottomPump_Channel1_Water_PFAS-inc-ddMS2"
  # ms2_pfas_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240802_C18neg_TopPump_Channel2_Water_PFAS-inc-ddMS2"
  # 
  # ms2_nonpfas_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18pos_BottomPump_Channel1_Water+PP-ddMS2"
  # ms2_nonpfas_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18neg_TopPump_Channel2_Water+PP-ddMS2"
  # 
  # ms2_pool_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18pos_BottomPump_Channel1_Water-ddMS2"
  # ms2_pool_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18neg_TopPump_Channel2_Water-ddMS2"
  
  # HILIC
  # reg_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240611_HILICpos_120k_BottomPump_Channel1"
  # reg_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240411_HILICneg_120k_TopPump_Channel2"
  # 
  # ms2_pfas_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240802_HILICpos_BottomPump_Channel1_Water_PFAS-inc-ddMS2"
  # ms2_pfas_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240802_HILICneg_TopPump_Channel2_Water_PFAS-inc-ddMS2"
  # 
  # ms2_nonpfas_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICpos_BottomPump_Channel1_Water+PP-ddMS2"
  # ms2_nonpfas_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICneg_TopPump_Channel2_Water+PP-ddMS2"
  # 
  # ms2_pool_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICpos_BottomPump_Channel1_Water-ddMS2"
  # ms2_pool_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICneg_TopPump_Channel2_Water-ddMS2"
  
  # Field 8 - Position
  # Front is shiny input, end comes from pos384 in df
  position <- paste0(pos, ":") # finish by row
  
  # Field 9 = Inj Vol
  if (machine == 'C18') {
    pos_vol <- "7.5"
    neg_vol <- "10"
  } else if (machine == 'HILIC') {
    pos_vol <- "5"
    neg_vol <- "5"
  } 
  
  # Field 10 - L1 Study
  l1 <- study
  # "CLU0120_MEC"
  
  # Field 11 - L3 Laboratory
  l3 <- tech
  
  # Field 12 - Sample Name
  # comes from dataframe by row
  
  #############################################################################
  # Count by sample type - needed for loops
  #############################################################################
  final_data$Sample_ID <- trimws(final_data$Sample_ID) #remove spaces 
  total_count <- sum(!is.na(final_data$Sample_ID)) #for checking
  
  ### CAL CURVES ############
  cal_curves <- final_data[grepl("FBS", final_data$Sample_ID), ] 
  cc_count <- nrow(cal_curves)
  
  ###  PFAS CAL CURVE ############
  pfas_ccs <- final_data[grepl("tbd?", final_data$Sample_ID), ] ## What is the name of the PFAS cal curve samples? ------------------------------
  pfas_cc_count <- nrow(pfas_ccs)
  
  ###  MDLS ############ --- DELETED 7/28 --- Keeping commented just in case
  # 5/29 - find MDL by finding duplicate cal_curves 
  # dup_sample_ids <- cal_curves$Sample_ID[duplicated(cal_curves$Sample_ID)]
  # # "FBS_0.1ng-mL"
  # dup_rows <- cal_curves[cal_curves$Sample_ID %in% dup_sample_ids, ] # two rows that have that ID
  # # Remove the duplicate with the higher value in Col_order_by_plate (later in the plate)
  # dup_to_remove <- dup_rows[ave(dup_rows$Col_order_by_plate, dup_rows$Sample_ID, FUN = max) == dup_rows$Col_order_by_plate, ]
  # #Remove the duplicate from cal_curves
  # cal_curves <- cal_curves[!cal_curves$ID %in% dup_to_remove$ID, ] # overwrite; count = 20
  # #assign duplicate to mdls
  # mdls <- dup_to_remove # count = 1
  
  ###  POOLS ############
  pools <- final_data[grepl("^HRE", final_data$Sample_ID), ] 
  pool_count <- nrow(pools) # should be 20
  # 5/29 - separate pools into pool 1, pool 2
  pools1 <- pools[grepl("^HRE\\.p1", pools$Sample_ID), ] 
  pools2 <- pools[grepl("^HRE\\.p2", pools$Sample_ID), ] 
  
  ###  WATERS ############
  waters <- final_data[grepl("^Water", final_data$Sample_ID), ]
  water_count <- nrow(waters)
  
  ###  AMAPS ############
  amaps <- final_data[grepl("^AM", final_data$Sample_ID), ]
  amap_count <- nrow(amaps)
  
  ###  NISTS ############
  nists <- final_data[grepl("^NIST", final_data$Sample_ID), ]
  nist_count <- nrow(nists)
  # OPTIONAL - if nist_count == 0
  
  ###  STUDY SAMPLES ############
  pattern_a <- "^A\\d{7}$"
  pattern_k <- "^K\\d{7}$"
  study_samples <- final_data[grepl(pattern_a, final_data$Sample_ID) | grepl(pattern_k, final_data$Sample_ID), ]
  study_sample_count <- nrow(study_samples)
  
  ### ORDER STUDY SAMPLES BASED ON USER INPUT changed 7/31 #################################
  order_column <- if (ordering_preference == "byrow") {
    "Row_order_by_plate"
  } else {
    "Col_order_by_plate"
  }
  
  ordered_study_samples <- final_data[order(final_data[[order_column]]), ]
  
  # 7/29: handle different sample sizes ####################################################
  # Expecting 80 samples per plate (2 plates)
  # If 60+: split in half
  # If <60: do only one chunk
  
  # Separate and group by study sample plate
  # Find all unique endings (last character) of pos96 column
  unique_endings <- study_samples %>%
    mutate(ending = str_sub(pos96, -1)) %>%
    pull(ending) %>%
    unique() %>%
    sort()
  
  ## For each rack number in study_samples (expecting 2),
  ## create new df rack#_samples with the samples from that rack only
  rack_samples_list <- list()
  
  for(ending in unique_endings) {
    # Filter samples and store in the list with a named entry
    rack_df <- study_samples %>% filter(str_ends(pos96, ending))
    rack_samples_list[[paste0("rack", ending, "_samples")]] <- rack_df
    cat(paste0("rack", ending, "_samples: ", nrow(rack_df), " rows\n"))
  }
  
  study_racks <- length(rack_samples_list)
  
  # Based on number of racks found, then size of each rack.
  if(study_racks == 2) {
    split1 <- TRUE
    cat("2 study racks found")
    rack1 <- rack_samples_list[[1]]
    rack1_num <- nrow(rack1)
    first_half_size <- rack1_num/2
    # If the first rack has less than 60 samples, do it all in one chunk
    if(rack1_num < 60) {
      split1 <- FALSE
      first_half_size <- rack1_num
    }
    
    split2 <- TRUE
    rack2 <- rack_samples_list[[2]]
    rack2_num <- nrow(rack2)
    second_half_size <- rack2_num/2
    # If the second rack has less than 60 samples, do it all in one chunk
    if(rack2_num < 60) {
      split2 <- FALSE
      second_half_size <- rack2_num
    }
    
  } else if (study_racks == 1) {
    cat("Only 1 study rack found")
    split1 <-TRUE
    rack1 <- rack_samples_list[[1]]
    rack1_num <- nrow(rack1)
    first_half_size <- rack1_num/2
    # If the rack rack has less than 60 samples, do it all in one chunk
    if(rack1_num < 60) {
      split1 <- FALSE
      first_half_size <- rack1_num
    }
  } else if (study_racks == 0) {
    cat("Error occurred. No study racks found in grouping process.")
  } else if (study_racks == 3) {
    cat("3 study racks found. Not coded yet!")
  }
  
  # Creates: (for a normal run):
  # rack1_samples: 80 obs
  # rack2_samples: 80 obs
  
  #Could be any of the following:
  #rack1_samples: 50 obs
  
  #rack1_samples: 80 obs
  #rack2_samples: 70 obs
  
  #rack1_samples: 80 obs
  #rack2 samples: 40 obs
  
  ## WHAT IF it's rack2_samples only?
  # Or rack2_samples and rack3_samples?
  # What if there are 3 study sample plates? (unlikely, but maybe later?) ----
  
  # If 60+: split in half
  # If <60: do only one chunk
  
  # Extra - count totals for next part and to check
  total_samples <- cc_count + water_count + amap_count + nist_count + study_sample_count
  
  if(total_count == total_samples) {
    print("All qaqcs and samples were identified!")
  } else {
    print(paste("Some samples were not identified. rows of dataframe =", total_count, "and sample types added together = ", total_samples))
  }
  # ^ for mismatch, print more info....?
  
  ####################################################################################
  
  # CREATE LINES 
  
  ####################################################################################
  
  # BLANKS lines ####################################################
  
  # For now: 4 blanks 
  # 2 at the beginning and end labeled 'Blank' 
  # 2 surrounding the Cal Curve labeled 'Cal_Std'
  
  num_blanks <- 4 # 4 blanks
  # Vector for storing lines
  blank_lines_pos <- vector("character", num_blanks)
  blank_lines_neg <- vector("character", num_blanks)
  
  # REAL BLANKS 
  first_blank_line_pos <- paste(
    sample_type,          # Field 1: Sample Type 
    filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
    paste0("Instrument_Blank_", batch,"_0", 1, "_1"),   # Field 3: Sample_ID_ID
    comment,              # Field 4: Comment
    l2,                   # Field 5: L2 Client
    path,                 # Field 6: Path
    reg_pos,           # Field 7: Instrument Method
    paste0("Y:A3"), # Field 8: Position: FIXED for blanks
    "0",                    # Field 9: Inj Vol
    l1,            # Field 10: L1 Study
    l3,                     # Field 11: L3 Laboratory
    "Blank",                # Field 12: Sample Name 
    sep = ","
  )
  
  blank_lines_pos[1] <- first_blank_line_pos
  
  first_blank_line_neg <- paste(
    sample_type,          # Field 1: Sample Type
    filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
    paste0("Instrument_Blank_", batch, "_0", 1, "_2"), # Field 3: Sample_ID_ID
    comment,              # Field 4: Comment
    l2,                   # Field 5: L2 Client
    path,                 # Field 6: Path
    reg_neg,           # Field 7: Instrument Method
    paste0("Y:A3"), # Field 8: Position: FIXED for blank
    "0",                    # Field 9: Inj Vol
    l1,            # Field 10: L1 Study
    l3,                     # Field 11: L3 Laboratory
    "Blank",                # Field 12: Sample Name 
    sep = ","
  )
  
  blank_lines_neg[1] <- first_blank_line_neg
  
  last_blank_line_pos <- paste(
    sample_type,          # Field 1: Sample Type 
    filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
    paste0("Instrument_Blank_", batch,"_0", num_blanks, "_1"),   # Field 3: Sample_ID_ID
    comment,              # Field 4: Comment
    l2,                   # Field 5: L2 Client
    path,                 # Field 6: Path
    reg_pos,           # Field 7: Instrument Method
    paste0("Y:A3"), # Field 8: Position: FIXED for blanks
    "0",                    # Field 9: Inj Vol
    l1,            # Field 10: L1 Study
    l3,                     # Field 11: L3 Laboratory
    "Blank",                # Field 12: Sample Name 
    sep = ","
  )
  
  blank_lines_pos[num_blanks] <- last_blank_line_pos
  
  last_blank_line_neg <- paste(
    sample_type,          # Field 1: Sample Type
    filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
    paste0("Instrument_Blank_", batch, "_0", num_blanks, "_2"), # Field 3: Sample_ID_ID
    comment,              # Field 4: Comment
    l2,                   # Field 5: L2 Client
    path,                 # Field 6: Path
    reg_neg,           # Field 7: Instrument Method
    paste0("Y:A3"), # Field 8: Position: FIXED for blank
    "0",                    # Field 9: Inj Vol
    l1,            # Field 10: L1 Study
    l3,                     # Field 11: L3 Laboratory
    "Blank",                # Field 12: Sample Name 
    sep = ","
  )
  
  blank_lines_neg[num_blanks] <- last_blank_line_neg
  
  # Not real 'BLANKS' Cal_Std - These surround the cal_curve ###################################################
  for (i in c(2,3)) {
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type 
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0("Instrument_Blank_", batch,"_0", i, "_1"),   # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0("Y:A3"), # Field 8: Position: FIXED for blanks
      "0",                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name #MANUALLY CHANGE FIRST AND LAST TO BLANK!
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0("Instrument_Blank_", batch, "_0", i, "_2"), # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0("Y:A3"), # Field 8: Position: FIXED for blank
      "0",                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name #MANUALLY CHANGE FIRST AND LAST TO BLANK!
      sep = ","
    )
    blank_lines_pos[i] <- pos_line
    blank_lines_neg[i] <- neg_line
  }
  
  # WATER lines ####################################################
  # Vector for storing lines
  water_lines_pos <- vector("character", nrow(waters))
  water_lines_neg <- vector("character", nrow(waters))
  
  for (i in 1:nrow(waters)) {
    
    # Needed information from data
    sample_id <- waters$Sample_ID[i]
    pos384 <- waters$pos384[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_0", i, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Water",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_0", i, "_2"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Water",                # Field 12: Sample Name
      sep = ","
    )
    water_lines_pos[i] <- pos_line
    water_lines_neg[i] <- neg_line
  }
  
  # NIST lines ####################################################
  # 7/28: MAKE OPTIONAL 
  if (nist_count == 0) {
    run_nist <- FALSE
  } else { # If nist_count > 0, execute the lines
    
    run_nist <- TRUE
    # Vector for storing lines
    nist_lines_pos <- vector("character", nrow(nists))
    nist_lines_neg <- vector("character", nrow(nists))
    
    for (i in 1:nrow(nists)) { #usually there will only be 1
      # Needed information from data
      sample_id <- nists$Sample_ID[i]
      pos384 <- nists$pos384[i]
      
      # For each well, create a positive and negative row
      pos_line <- paste(
        sample_type,          # Field 1: Sample Type
        filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
        paste0(sample_id, "_", batch, "_1"),          # Field 3: Sample_ID_ID
        comment,              # Field 4: Comment
        l2,                   # Field 5: L2 Client
        path,                 # Field 6: Path
        reg_pos,           # Field 7: Instrument Method
        paste0(position, pos384), # Field 8: Position  
        pos_vol,                    # Field 9: Inj Vol
        l1,            # Field 10: L1 Study
        l3,                     # Field 11: L3 Laboratory
        "NIST",                # Field 12: Sample Name
        sep = ","
      )
      
      neg_line <- paste(
        sample_type,          # Field 1: Sample Type
        filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
        paste0(sample_id, "_", batch, "_2"),          # Field 3: Sample_ID_ID
        comment,              # Field 4: Comment
        l2,                   # Field 5: L2 Client
        path,                 # Field 6: Path
        reg_neg,           # Field 7: Instrument Method
        paste0(position, pos384), # Field 8: Position  
        neg_vol,                    # Field 9: Inj Vol
        l1,            # Field 10: L1 Study
        l3,                     # Field 11: L3 Laboratory
        "NIST",                # Field 12: Sample Name
        sep = ","
      )
      nist_lines_pos[i] <- pos_line
      nist_lines_neg[i] <- neg_line
    }
    
  } # END ELSE
  
  # AMAP lines ####################################################
  # Vector for storing lines
  amap_lines_pos <- vector("character", nrow(amaps))
  amap_lines_neg <- vector("character", nrow(amaps))
  
  for (i in 1:nrow(amaps)) {
    
    # Add run order if manually adding AMAPS (used 5/15/25)
    # 1 3 --> 1 ; 2, 4 --> 3 ; 5 --> 2
    #amaps$runorder = c(1, 3, 1, 3, 2)
    
    # Needed information from data
    sample_id <- amaps$Sample_ID[i]
    pos384 <- amaps$pos384[i]
    #x <- amaps$runorder[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_0", i, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "AMAP",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_0", i, "_2"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "AMAP",                # Field 12: Sample Name
      sep = ","
    )
    amap_lines_pos[i] <- pos_line
    amap_lines_neg[i] <- neg_line
  }
  
  # POOL lines ####################################################
  ## FROM ABOVE: 
  # pool_count is all pools, used to be 20, now 12
  # pools1 <- pools[grepl("^HRE\\.p1", pools$Sample_ID), ] # used to be 10, now 6
  # pools2 <- pools[grepl("^HRE\\.p2", pools$Sample_ID), ] # used to be 10, now 6
  
  # Check for 20 pools
  if (pool_count == 20) {
    pools1$runorder <- sprintf("%02d", 1:10)
    pools2$runorder <- sprintf("%02d", 1:10)
    cat("Found 20 pools")
    #} else {
    #warning(paste("Expected 20 pools but found", pool_count, "- runorder not assigned"))
  }
  
  # New - check for 12 pools
  if (pool_count == 12) {
    pools1$runorder <- sprintf("%02d", 1:6)
    pools2$runorder <- sprintf("%02d", 1:6)
    cat("Found 12 pools")
  } else {
    warning(paste("Expected 12 pools but found", pool_count, "- runorder not assigned"))
  }
  
  ############ POOL 1 ################################
  
  # Vector for storing lines
  pool1_lines_pos <- vector("character", nrow(pools1)) 
  pool1_lines_neg <- vector("character", nrow(pools1))
  
  for (i in 1:nrow(pools1)) {
    # Needed information from data
    sample_id <- pools1$Sample_ID[i]
    pos384 <- pools1$pos384[i]
    order <- pools1$runorder[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_", order, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "HRE_Pool",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_", order, "_2"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "HRE_Pool",                # Field 12: Sample Name
      sep = ","
    )
    pool1_lines_pos[i] <- pos_line
    pool1_lines_neg[i] <- neg_line
  }
  
  ############ POOL 2 ################################
  
  # Vector for storing lines
  pool2_lines_pos <- vector("character", nrow(pools2)) 
  pool2_lines_neg <- vector("character", nrow(pools2))
  
  for (i in 1:nrow(pools1)) {
    # Needed information from data
    sample_id <- pools2$Sample_ID[i]
    pos384 <- pools2$pos384[i]
    order <- pools2$runorder[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_", order, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "HRE_Pool",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_", order, "_2"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "HRE_Pool",                # Field 12: Sample Name
      sep = ","
    )
    pool2_lines_pos[i] <- pos_line
    pool2_lines_neg[i] <- neg_line
  }
  
  # Cal Curve lines ###################################################
  # Vector for storing lines
  cal_curve_lines_pos <- vector("character", nrow(cal_curves)) #cal_curve_lines_pos is chr [1:8]
  cal_curve_lines_neg <- vector("character", nrow(cal_curves))
  
  for (i in 1:nrow(cal_curves)) { # should be 8!
    # Needed information from data
    sample_id <- cal_curves$Sample_ID[i]
    pos384 <- cal_curves$pos384[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_2"),        # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name
      sep = ","
    )
    cal_curve_lines_pos[i] <- pos_line
    cal_curve_lines_neg[i] <- neg_line
  }
  
  # New PFAS Cal Curve lines ################################################### Added 7/28
  # Vector for storing lines
  pfas_cc_lines_pos <- vector("character", nrow(cal_curves)) #cal_curve_lines_pos is chr [1:8]
  pfas_cc_lines_neg <- vector("character", nrow(cal_curves))
  
  for (i in 1:nrow(pfas_ccs)) { # should be 8!
    # Needed information from data
    sample_id <- pfas_ccs$Sample_ID[i]
    pos384 <- pfas_ccs$pos384[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_1"),    # Field 3: Sample_ID_ID ## NUMBERING? There are 3 groups of 0.05, 0.1, 0.5? ----------------------
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name ## WHAT TO NAME THIS? ----------------------
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_", batch, "_2"),    # Field 3: Sample_ID_ID ## NUMBERING? There are 3 groups of 0.05, 0.1, 0.5? ----------------------
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Cal_Std",                # Field 12: Sample Name ## WHAT TO NAME THIS? ----------------------
      sep = ","
    )
    pfas_cc_lines_pos[i] <- pos_line
    pfas_cc_lines_neg[i] <- neg_line
  }
  
  # # MDL line ###################################################
  # # --DELETED MSL --- Removed 7/28; leaving in case of a later need
  # # Expect mdls = 1 row with the extra cal curve
  # 
  # # Vector for storing lines - just in case there is more than 1
  # mdl_lines_pos <- vector("character", nrow(mdls))
  # mdl_lines_neg <- vector("character", nrow(mdls))
  # 
  # for (i in 1:nrow(mdls)) { # should be 1
  #   
  #   sample_id <- mdls$Sample_ID[i]
  #   pos384 <- mdls$pos384[i]
  #   
  #   # For each well, create a positive and negative row
  #   pos_line <- paste(
  #     sample_type,          # Field 1: Sample Type
  #     filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
  #     paste0(sample_id, "_", batch, "_1"),          # Field 3: Sample_ID_ID
  #     comment,              # Field 4: Comment
  #     l2,                   # Field 5: L2 Client
  #     path,                 # Field 6: Path
  #     reg_pos,           # Field 7: Instrument Method
  #     paste0(position, pos384), # Field 8: Position  
  #     pos_vol,                    # Field 9: Inj Vol
  #     l1,            # Field 10: L1 Study
  #     l3,                     # Field 11: L3 Laboratory
  #     "MDL",                # Field 12: Sample Name
  #     sep = ","
  #   )
  #   
  #   neg_line <- paste(
  #     sample_type,          # Field 1: Sample Type
  #     filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
  #     paste0(sample_id, "_", batch, "_2"),        # Field 3: Sample_ID_ID
  #     comment,              # Field 4: Comment
  #     l2,                   # Field 5: L2 Client
  #     path,                 # Field 6: Path
  #     reg_neg,           # Field 7: Instrument Method
  #     paste0(position, pos384), # Field 8: Position  
  #     neg_vol,                    # Field 9: Inj Vol
  #     l1,            # Field 10: L1 Study
  #     l3,                     # Field 11: L3 Laboratory
  #     "MDL",                # Field 12: Sample Name
  #     sep = ","
  #   )
  #   
  #   mdl_lines_pos[i] <- pos_line
  #   mdl_lines_neg[i] <- neg_line
  # }
  
  # SAMPLE LINES ######################################################
  # No grouping here; simply generate all the lines for all study samples.
  # Vector for storing lines
  study_sample_lines_pos <- vector("character", nrow(study_samples))
  study_sample_lines_neg <- vector("character", nrow(study_samples))
  
  for (i in 1:nrow(study_samples)) {
    
    # Needed information from data
    sample_id <- study_samples$Sample_ID[i]
    pos384 <- study_samples$pos384[i]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_1"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Study_Sample",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER when printing.
      paste0(sample_id, "_2"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      path,                 # Field 6: Path
      reg_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "Study_Sample",                # Field 12: Sample Name
      sep = ","
    )
    study_sample_lines_pos[i] <- pos_line
    study_sample_lines_neg[i] <- neg_line
  }
  
  ###############################################################################################
  # MSMS Lines
  ###############################################################################################
  
  # First/Last batch: Special MSMS on HRE_Pool (code later)
  # PFAS-MSMS 6 times
  # non-PFAS MSMS 6 times
  
  # CHOOSE POSITIONS MANUALLY - MUST BE SAMPLE POSITIONS #################################################
  pfas_ms2_positions <- c("C3", "E17", "G19", "C10", "E4", "M12")
  nonpfas_ms2_positions <- c("E11", "G17", "C16", "I14", "M8", "O20")
  
  # ^^^^ Maybe in the app some day you can select which samples/positions you want to run these on -------------------------------------------
  # Or have the option to choose random?
  
  # check they are study sample positions below ###########################
  
  all_positions <- c(pfas_ms2_positions, nonpfas_ms2_positions)
  
  cat("Printing positions for MSMS locations (verify all are study samples):\n")
  
  # Loop through each position and print the sampletype
  for (pos in all_positions) {
    # Find the row where pos384 equals the current position
    row_index <- which(final_data$pos384 == pos)
    # Check if the position exists in the data
    if (length(row_index) > 0) {
      # Print the position and its corresponding sampletype
      cat("Position:", pos, "| Sample Type:", final_data$sampletype[row_index], "\n")
    } else {
      cat("Position:", pos, "| Not found in data\n")
    }
  }
  
  ###########################
  # Add a marker to know whether the study sample is an MSMS position
  study_samples$pfms <- FALSE
  rows <- which(study_samples$pos384 %in% pfas_ms2_positions)
  study_samples$pfms[rows] <- TRUE
  
  study_samples$nonpfms <- FALSE
  rows <- which(study_samples$pos384 %in% nonpfas_ms2_positions)
  study_samples$nonpfms[rows] <- TRUE
  
  #### PFAS MSMS LINES ####################################
  # Vector for storing lines
  pfas_msms_lines_pos <- vector("character", 6)
  pfas_msms_lines_neg <- vector("character", 6)
  
  for (i in 1:6) {
    pos384 <- pfas_ms2_positions[i]
    
    # find matrixID for this position from study_samples
    row <- which(study_samples$pos384 == pos384)
    sample_id <- study_samples$ID[row]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER AND PFAS-MSMS AT END
      paste0(sample_id, "_1_PFAS-MSMS"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      ms2path,                 # Field 6: Path
      ms2_pfas_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "PFAS-MSMS",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER AND PFAS-MSMS AT END
      paste0(sample_id, "_2_PFAS-MSMS"),        # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      ms2path,                 # Field 6: Path
      ms2_pfas_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "PFAS-MSMS",                # Field 12: Sample Name
      sep = ","
    )
    pfas_msms_lines_pos[i] <- pos_line
    pfas_msms_lines_neg[i] <- neg_line
  }
  
  # Non-PFAS MSMS LINES #########################################
  # Vector for storing lines
  nonpfas_msms_lines_pos <- vector("character", 6)
  nonpfas_msms_lines_neg <- vector("character", 6)
  
  for (i in 1:6) {
    pos384 <- nonpfas_ms2_positions[i]
    # find matrix ID for this position from study_samples
    row <- which(study_samples$pos384 == pos384)
    sample_id <- study_samples$ID[row]
    
    # For each well, create a positive and negative row
    pos_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_pos,             # Field 2: File Name NEEDS ORDER NUMBER AND _MSMS AT END
      paste0(sample_id, "_1_MSMS"),          # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      ms2path,                 # Field 6: Path
      ms2_nonpfas_pos,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      pos_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "MSMS",                # Field 12: Sample Name
      sep = ","
    )
    
    neg_line <- paste(
      sample_type,          # Field 1: Sample Type
      filename_neg,             # Field 2: File Name NEEDS ORDER NUMBER AND _MSMS AT END
      paste0(sample_id, "_2_MSMS"),        # Field 3: Sample_ID_ID
      comment,              # Field 4: Comment
      l2,                   # Field 5: L2 Client
      ms2path,                 # Field 6: Path
      ms2_nonpfas_neg,           # Field 7: Instrument Method
      paste0(position, pos384), # Field 8: Position  
      neg_vol,                    # Field 9: Inj Vol
      l1,            # Field 10: L1 Study
      l3,                     # Field 11: L3 Laboratory
      "MSMS",                # Field 12: Sample Name
      sep = ","
    )
    nonpfas_msms_lines_pos[i] <- pos_line
    nonpfas_msms_lines_neg[i] <- neg_line
  }
  
  ###################################################################################################################################
  # Functions to help modifying as we write lines 
  ###################################################################################################################################
  
  # MODIFY Function:
  # appends run order to the end of the file name
  # appends rack number based on position (first or second half)
  # appends MSMS-specific suffixes
  
  total_runs <- total_count # hardcode?
  
  modify <- function(line, run_order, msms_type = NULL) {
    # Split the line by commas
    parts <- strsplit(line, ",")[[1]]
    
    # Format run order as a 3-digit string
    formatted_run_order <- sprintf("%03d", run_order)
    
    # First append the run order to field 2 (filename)
    parts[2] <- paste0(parts[2], formatted_run_order)
    
    # If this is an MSMS run, append the appropriate suffix
    if (!is.null(msms_type)) {
      if (msms_type == "PFAS") {
        parts[2] <- paste0(parts[2], "_PFAS-MSMS")
      } else if (msms_type == "nonPFAS") {
        parts[2] <- paste0(parts[2], "_MSMS")
      }
    }
    
    # Determine if we're in first half or second half
    half_point <- ceiling(total_runs / 2)
    
    # Choose the appropriate l2 value (assumed to be in field 5)
    if (run_order <= half_point) {
      l2_value <- sub(",.*", "", rack)
    } else {
      l2_value <- sub(".*?,", "", rack)
    }
    
    # Update L2 field (assumed to be in position 5)
    parts[5] <- l2_value
    
    
    # Rejoin the parts with commas
    paste(parts, collapse = ",")
  }
  
  ##### WRITE FILE ###########################################################################################
  # 7/28 change to new order:
  # 1 real blank (front and back labeled 'Blank', middle labeled 'Cal_Std')
  # 1 water (out of 2)
  # 1 NIST (OPTIONAL, out of 1)
  
  # Pool 1 
  # Pool 2
  # 3 PFAS (0.1, 0.5, 1)
  # 40 study samples
  # Pool 1
  # Pool 2
  # 40 study samples
  # Pool 1
  # Pool 2
  # 3 PFAS (0.1, 0.5, 1)
  
  # 1 'blank' 
  # CAL CURVE (1-8/8)
  # 1 'blank' 
  
  # Pool 1
  # Pool 2
  # 40 study samples
  # Pool 1
  # Pool 2
  # 40 study samples
  # Pool 1
  # Pool 2
  # 3 PFAS (0.1, 0.5, 1)
  # 1 AMAP (out of 1)
  # 1 water (out of 2)
  # 1 real blank
  
  ### HANDLING
  # Expecting 80 samples per plate (2 plates)
  # If 60+: split in half
  # If <60: do only one chunk
  
  ## INITIALIZE
  # Create a connection to the file
  file_conn <- file(sequence_filepath, "w") #from shiny
  # "w" to overwrite or create, "a" to append
  
  #####################################################################################################################################################################################
  # START WRITING
  #####################################################################################################################################################################################
  run_order <- 1
  
  # Headers
  # Write the two-row header format to the file
  writeLines("Bracket Type=4,,,,,,,,,,,", file_conn)
  writeLines("Sample Type,File Name,Sample ID,Comment,L2 Client,Path,Instrument Method,Position,Inj Vol,L1 Study,L3 Laboratory,Sample Name", file_conn)
  
  # 1 instrument blank - actual blank
  writeLines(modify(blank_lines_pos[1], run_order), file_conn)
  writeLines(modify(blank_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
  
  # 1 water blank - 1 of 2
  writeLines(modify(water_lines_pos[1], run_order), file_conn)
  writeLines(modify(water_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
  
  # 1 NIST - 7/29 changed to OPTIONAL
  if (run_nist) {
    # only execute if there is a NIST sample
    writeLines(modify(nist_lines_pos[1], run_order), file_conn)
    writeLines(modify(nist_lines_neg[1], run_order), file_conn)
    run_order <- run_order + 1
  } else {
    cat("No NIST sample identified in batch.\n")
  }
  
  ####### POOL ##########################################
  # separated by pool number 5/29
  
  writeLines(modify(pool1_lines_pos[1], run_order), file_conn) # Pool 1
  writeLines(modify(pool1_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
  
  writeLines(modify(pool2_lines_pos[1], run_order), file_conn) # Pool 2
  writeLines(modify(pool2_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
  
  ## 7/28?? ADD 3 PFAS CURVE HERE (1 of 3)
  
  ### Note: MSMS LOGIC ###############################
  # When writing study samples, if
  
  # study_samples$pfms == TRUE 
  # then there is a PFAS MSMS run after that sample
  # study_samples$nonpfms == TRUE
  # then there is a nonPFAS MSMS run after that sample.
  
  ## Sample size logic: 7/29
  
  # if study_racks == 2 or 1
  # if 2, split = c(T/F, T/F)
  # if 1, split = T/F
  
  ## 7/28, 7/29 NEW: 1 of 4 groups of samples
  
  batch_size <- first_half_size #40 expected
  
  # If first half is split, batch_size is 80/2 = 40 
  # If first half is <60, batch_size is that number. Ex: 50
  
  for (i in 1:batch_size) { #This is correct ################################################################
    # If only one chunk, batch size is the full first batch
    # If two chunks, batch size is half the full first batch
    writeLines(modify(study_sample_lines_pos[i], run_order), file_conn)
    writeLines(modify(study_sample_lines_neg[i], run_order), file_conn)
    
    # If this sample is flagged for PFAS MSMS, add those lines
    if (study_samples$pfms[i] == TRUE) {
      
      # Find which position in the pfas_ms2_positions list this sample corresponds to
      pos_index <- which(pfas_ms2_positions == study_samples$pos384[i])
      
      writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
      writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
    }
    
    # Same for non-PFAS MSMS 
    if (study_samples$nonpfms[i] == TRUE) {
      
      # Find which position in the pfas_ms2_positions list this sample corresponds to
      pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[i])
      
      writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
      writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
    }
    #Increment 
    run_order <- run_order + 1
  }
  ################################################################################## End study sample chunk 1 of 4
  
  writeLines(modify(pool1_lines_pos[2], run_order), file_conn) # Pool 1
  writeLines(modify(pool1_lines_neg[2], run_order), file_conn)
  run_order <- run_order + 1
  
  writeLines(modify(pool2_lines_pos[2], run_order), file_conn) # Pool 2
  writeLines(modify(pool2_lines_neg[2], run_order), file_conn)
  run_order <- run_order + 1
  
  # If <60 samples in the first rack: SKIP until the 1st PFAS curve
  if (split1) { # If split1 is false, do first part in one batch. Skip until the PFAS curve
    # 2 of 4 groups of 40 study samples (batch_size = 40) #########################################################
    for (i in ((batch_size+1):(batch_size*2))) { #41-80
      
      writeLines(modify(study_sample_lines_pos[i], run_order), file_conn)
      writeLines(modify(study_sample_lines_neg[i], run_order), file_conn)
      
      # If this sample is flagged for PFAS MSMS, add those lines
      if (study_samples$pfms[i] == TRUE) {
        
        # Find which position in the pfas_ms2_positions list this sample corresponds to
        pos_index <- which(pfas_ms2_positions == study_samples$pos384[i])
        
        writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
        writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
      }
      
      # Same for non-PFAS MSMS 
      if (study_samples$nonpfms[i] == TRUE) {
        
        # Find which position in the pfas_ms2_positions list this sample corresponds to
        pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[i])
        
        writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
        writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
      }
      run_order <- run_order + 1
    }
    ################################################################################## End study sample chunk 2 of 4
    
    writeLines(modify(pool1_lines_pos[3], run_order), file_conn) # Pool 1
    writeLines(modify(pool1_lines_neg[3], run_order), file_conn)
    run_order <- run_order + 1
    
    writeLines(modify(pool2_lines_pos[3], run_order), file_conn) # Pool 2
    writeLines(modify(pool2_lines_neg[3], run_order), file_conn)
    run_order <- run_order + 1
    
  }
  
  ## 7/28?? ADD 3 PFAS CURVE HERE (2 of 3)
  
  
  ################################################################################
  
  # # 1 instrument blank - before Cal Curve
  writeLines(modify(blank_lines_pos[2], run_order), file_conn) 
  writeLines(modify(blank_lines_neg[2], run_order), file_conn)
  run_order <- run_order + 1
  
  # CAL CURVE !!!! # changed to loop 5/29
  for (i in 1:length(cal_curve_lines_pos)) { # should be 8
    writeLines(modify(cal_curve_lines_pos[i], run_order), file_conn)
    writeLines(modify(cal_curve_lines_neg[i], run_order), file_conn)
    run_order <- run_order + 1
  }
  
  # 1 instrument blank - after cal curve
  writeLines(modify(blank_lines_pos[3], run_order), file_conn)
  writeLines(modify(blank_lines_neg[3], run_order), file_conn)
  run_order <- run_order + 1
  
  ################################################################################
  
  ## SKIP TO 3rd PFAS curve if only one study rack
  if (study_racks == 2) {
    
    writeLines(modify(pool1_lines_pos[4], run_order), file_conn) # Pool 1
    writeLines(modify(pool1_lines_neg[4], run_order), file_conn)
    run_order <- run_order + 1
    
    writeLines(modify(pool2_lines_pos[4], run_order), file_conn) # Pool 2
    writeLines(modify(pool2_lines_neg[4], run_order), file_conn)
    run_order <- run_order + 1
    
    batch_size <- second_half_size 
    # Again, this only runs if there are two study racks.
    # Again, batch_size will be the FULL second rack if there are less than 60 samples in the rack.
    # batch_size will be HALF the second rack if there are 60 or more samples.
    
    # 3 of 4 groups of 40 study samples (batch_size = 40) ######################################
    for (i in ((2*batch_size+1):(batch_size*3))) { #81-120
      
      writeLines(modify(study_sample_lines_pos[i], run_order), file_conn)
      writeLines(modify(study_sample_lines_neg[i], run_order), file_conn)
      
      # If this sample is flagged for PFAS MSMS, add those lines
      if (study_samples$pfms[i] == TRUE) {
        
        # Find which position in the pfas_ms2_positions list this sample corresponds to
        pos_index <- which(pfas_ms2_positions == study_samples$pos384[i])
        
        writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
        writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
      }
      
      # Same for non-PFAS MSMS 
      if (study_samples$nonpfms[i] == TRUE) {
        
        # Find which position in the pfas_ms2_positions list this sample corresponds to
        pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[i])
        
        writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
        writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
      }
      run_order <- run_order + 1
    }
    ################################################################################## End study sample chunk 3 of 4
    
    if (split2) {
      writeLines(modify(pool1_lines_pos[5], run_order), file_conn) # Pool 1
      writeLines(modify(pool1_lines_neg[5], run_order), file_conn)
      run_order <- run_order + 1
      
      writeLines(modify(pool2_lines_pos[5], run_order), file_conn) # Pool 2
      writeLines(modify(pool2_lines_neg[5], run_order), file_conn)
      run_order <- run_order + 1
      
      # 4 of 4 groups of 40 study samples (batch_size = 40) #######################################
      for (i in ((3*batch_size+1):(batch_size*4))) { #121-160
        
        writeLines(modify(study_sample_lines_pos[i], run_order), file_conn)
        writeLines(modify(study_sample_lines_neg[i], run_order), file_conn)
        
        # If this sample is flagged for PFAS MSMS, add those lines
        if (study_samples$pfms[i] == TRUE) {
          
          # Find which position in the pfas_ms2_positions list this sample corresponds to
          pos_index <- which(pfas_ms2_positions == study_samples$pos384[i])
          
          writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
          writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
        }
        
        # Same for non-PFAS MSMS 
        if (study_samples$nonpfms[i] == TRUE) {
          
          # Find which position in the pfas_ms2_positions list this sample corresponds to
          pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[i])
          
          writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
          writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
        }
        run_order <- run_order + 1
      }
      ################################################################################## End study sample chunk 4 of 4
    } 
    
    # RETURN here if only one chunk for second half
    writeLines(modify(pool1_lines_pos[6], run_order), file_conn) # Pool 1
    writeLines(modify(pool1_lines_neg[6], run_order), file_conn)
    run_order <- run_order + 1
    
    writeLines(modify(pool2_lines_pos[6], run_order), file_conn) # Pool 2
    writeLines(modify(pool2_lines_neg[6], run_order), file_conn)
    run_order <- run_order + 1
    
  } # RESUME if only one study rack.
  
  ## 7/28?? ADD 3 PFAS CURVE HERE (3 of 3)
  
  # 1 AMAP - out of 1
  writeLines(modify(amap_lines_pos[1], run_order), file_conn) 
  writeLines(modify(amap_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
  
  # MDL --- DELETED --- keeping commented in case 
  # 1 MDL - extra cal curve (auto-selected, comment out if re-instating this)
  # for (i in 1:nrow(mdls)) { #should be 1
  #   writeLines(modify(mdl_lines_pos[i], run_order), file_conn)
  #   writeLines(modify(mdl_lines_neg[i], run_order), file_conn)
  #   run_order <- run_order + 1
  # }
  
  # 1 water blank - out of 2
  writeLines(modify(water_lines_pos[2], run_order), file_conn)
  writeLines(modify(water_lines_neg[2], run_order), file_conn)
  run_order <- run_order + 1
  
  # 1 instrument blank - actual blank, num_blanks = 4
  writeLines(modify(blank_lines_pos[num_blanks], run_order), file_conn) #4 blanks
  writeLines(modify(blank_lines_neg[num_blanks], run_order), file_conn)
  #######################################################################################################################
  
  close(file_conn)
  gc() # Force garbage collector helps release lingering file handles
  # So you can open without read-only
  
  return(invisible(TRUE)) # this big function is not meant to return anyting, just generate csv
}

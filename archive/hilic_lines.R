####################################################################################################################
#                       NOW ADAPTED TO C18 OR HILIC
####################################################################################################################

####################################################################################################################
#For now: 
# 4 instrument blanks. 
num_blanks <- 4
  # 2 at the beginning and end labeled 'Blank' 
  # 2 surrounding the Cal Curve labeled 'Cal_Std'

# REAL BLANKS --------------------------------------------------------
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

# 'BLANKS' Cal_Std - These surround the cal_curve ###################################################
blank_lines_pos <- vector("character", num_blanks)
blank_lines_neg <- vector("character", num_blanks)

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

# 5/29 - Making more robust
# pools1 - 10 rows
# pools2 - 10 rows

# needs 20 - hard code - fix later?
if (pool_count == 20) {
  pools1$runorder <- sprintf("%02d", 1:10)
  pools2$runorder <- sprintf("%02d", 1:10)
} else {
  warning(paste("Expected 20 pools but found", pool_count, "- runorder not assigned"))
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

# MDL line ###################################################
# changing to be more robust 5/29
# mdls = should be 1 row with the extra cal curve

# Vector for storing lines - just in case there is more than 1
mdl_lines_pos <- vector("character", nrow(mdls))
mdl_lines_neg <- vector("character", nrow(mdls))

for (i in 1:nrow(mdls)) { # should be 1
  
  sample_id <- mdls$Sample_ID[i]
  pos384 <- mdls$pos384[i]
  
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
    "MDL",                # Field 12: Sample Name
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
    "MDL",                # Field 12: Sample Name
    sep = ","
  )
  
  mdl_lines_pos[i] <- pos_line
  mdl_lines_neg[i] <- neg_line
}

# SAMPLE LINES ######################################################
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
# Maybe in the app some day you can select which samples/positions you want to run these on 
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

## All study samples ############################################################################

#### PFAS MSMS ####################################
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

# Non-PFAS MSMS #########################################
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

total_runs <- 197 # hardcode?

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

# The order Catherine and I discussed is as follows:
# Each is two rows - one pos, one neg. 

# 1 real blank (front and back labeled 'Blank', middle labeled 'Cal_Std')
# 1 water (out of 2)
# 1 NIST (out of 1)
# 1 AMAP  (out of 2)

# Pool 1 
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples
# Pool 1
# Pool 2 

# 1 'blank' 
# CAL CURVE (1-8/8)
# 1 'blank' 

# Pool 1 (6/10)
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples
# Pool 1
# Pool 2
# 20 study samples (160 total)
# Pool 1
# Pool 2 

# 1 AMAP (2/2)
# MDL (Cal curve)
# Water (2/2)
# 1 real blank 

## INITIALIZE 
# Sample batch size
batch_size <- 20 

# Create a connection to the file
file_conn <- file(sequence_filepath, "w") #from shiny
# "w" to overwrite or create, "a" to append

###############################################################################################
# START WRITING
###############################################################################################
run_order <- 1

# Headers
# Write the two-row header format to the file
writeLines("Bracket Type=4,,,,,,,,,,,", file_conn)
writeLines("Sample Type,File Name,Sample ID,Comment,L2 Client,Path,Instrument Method,Position,Inj Vol,L1 Study,L3 Laboratory,Sample Name", file_conn)

# 1 instrument blank - actual blank
writeLines(modify(first_blank_line_pos, run_order), file_conn)
writeLines(modify(first_blank_line_neg, run_order), file_conn)
run_order <- run_order + 1

# 1 water blank
writeLines(modify(water_lines_pos[1], run_order), file_conn)
writeLines(modify(water_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

# 1 NIST
writeLines(modify(nist_lines_pos[1], run_order), file_conn)
writeLines(modify(nist_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

# 1 AMAP
writeLines(modify(amap_lines_pos[1], run_order), file_conn) 
writeLines(modify(amap_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

####### POOL ##########################################
# separated by pool number 5/29

writeLines(modify(pool1_lines_pos[1], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[1], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

### MSMS LOGIC ###############################
# When writing study samples, if

# study_samples$pfms == TRUE 
  # then there is a PFAS MSMS run after that sample
# study_samples$nonpfms == TRUE
  # then there is a nonPFAS MSMS run after that sample.

# 1 of 8 groups of 20 study samples (batch_size = 20)
for (i in 1:batch_size) {
  
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

writeLines(modify(pool1_lines_pos[2], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[2], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

# 2 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((batch_size+1):(batch_size*2))) {
  
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

writeLines(modify(pool1_lines_pos[3], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[3], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[3], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[3], run_order), file_conn)
run_order <- run_order + 1

# 3 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((2*batch_size+1):(batch_size*3))) {
  
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

writeLines(modify(pool1_lines_pos[4], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[4], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[4], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[4], run_order), file_conn)
run_order <- run_order + 1

# 4 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((3*batch_size+1):(batch_size*4))) {
  
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

writeLines(modify(pool1_lines_pos[5], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[5], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[5], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[5], run_order), file_conn)
run_order <- run_order + 1

################################################################################

# # 1 instrument blank - before Cal Curve
writeLines(modify(blank_lines_pos[2], run_order), file_conn) 
writeLines(modify(blank_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

# Cal curve! # changed to loop 5/29
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

# Pool: continue separated

writeLines(modify(pool1_lines_pos[6], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[6], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[6], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[6], run_order), file_conn)
run_order <- run_order + 1

# 5 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((4*batch_size+1):(batch_size*5))) {
  
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

writeLines(modify(pool1_lines_pos[7], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[7], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[7], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[7], run_order), file_conn)
run_order <- run_order + 1

# 6 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((5*batch_size+1):(batch_size*6))) {
  
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

writeLines(modify(pool1_lines_pos[8], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[8], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[8], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[8], run_order), file_conn)
run_order <- run_order + 1

# 7 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((6*batch_size+1):(batch_size*7))) {
  
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

writeLines(modify(pool1_lines_pos[9], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[9], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[9], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[9], run_order), file_conn)
run_order <- run_order + 1

# 8 of 8 groups of 20 study samples (batch_size = 20)
for (i in ((7*batch_size+1):(batch_size*8))) {
  
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

writeLines(modify(pool1_lines_pos[10], run_order), file_conn) # Pool 1
writeLines(modify(pool1_lines_neg[10], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[10], run_order), file_conn) # Pool 2
writeLines(modify(pool2_lines_neg[10], run_order), file_conn)
run_order <- run_order + 1

##

# 1 AMAP
writeLines(modify(amap_lines_pos[2], run_order), file_conn) 
writeLines(modify(amap_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

#1 MDL - the last cal curve qaqc sample - fixed 5/29

for (i in 1:nrow(mdls)) { #should be 1
  writeLines(modify(mdl_lines_pos[i], run_order), file_conn)
  writeLines(modify(mdl_lines_neg[i], run_order), file_conn)
  run_order <- run_order + 1
}

# 1 water blank
writeLines(modify(water_lines_pos[2], run_order), file_conn)
writeLines(modify(water_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

# 1 instrument blank - actual blank
writeLines(modify(last_blank_line_pos, run_order), file_conn)
writeLines(modify(last_blank_line_neg, run_order), file_conn)

#######################################################################################################################

close(file_conn)
gc() # Force garbace collector helps release lingering file handles
# So you can open without read-only
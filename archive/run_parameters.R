# id_container() is called final_data

############# VARIABLES ########################
# date <- '250515'
# study <- 'CLU0120_MEC'
# batch <- '04'
# rack <- '7,8'
# tech <- 'CM'
# machine <- 'C18' 
# pos <- 'G'
# # path 'E:/...' not implemented yet
# # firstlast T/F not implemented yet
# # 
# sequence_filename <- "CLU_test"
# sequence_filepath <- "test/path"
# 
# final_data <- final_data 



# TRIM WHITE SPACE 
final_data$Sample_ID <- trimws(final_data$Sample_ID, which = "left")
# other rows ?

# Inputs from shiny:
    # date = '250512'
    # study = 'CLU0120_MEC'
    # batch = 02
    # tech = 'MC'
    # machine = 'C18' OR 'HILIC'
    # pos = "B", "R", "G", or "Y"
    # firstlast = TRUE/FALSE

# Variables needed #######################################
# Field 1
sample_type <- 'Unknown' #always Unknown (for now?)

# Field 2 - File Name
# R or N?
if (machine == 'C18') {
  letter <- 'R'
} else if (machine == 'HILIC') {
  letter <- 'N'
} 

studynum <- sub("_.*$", "", study)

filename_pos <- sprintf("%s%s_%s_%spos_", letter, date, studynum, machine)
# "R250512_CLU0120_C18pos_" --> add 3-digit number in order when printing
filename_neg <- sprintf("%s%s_%s_%sneg_", letter, date, studynum, machine)
#### WILL NEED TO ADD THE 3-DIGIT RUN ORDER NUMBER (SAME FOR POS/NEG and MSMS)

# Field 3 - Sample ID 
# comes from dataframe by row


  
# Field 4 - Comment 
comment <- as.numeric(batch)

# Field 5 - L2 Client
l2 <- "add"

# Field 6 - Path - this will be selectable later on....

endpath <- sprintf("Batch_%s_%s", batch, date)

if (machine == 'C18') {
  
  path <- sprintf('E:/Projects/CLU0120_250501_MEC_C18/1-Raw_Files/%s/', endpath)
  # "E:/Projects/CLU0120_250501_MEC_C18/1-Raw_Files/Batch_4_250515/"
  ms2path <- paste0(path, '/MSMS')
  # "E:/Projects/CLU0120_250501_MEC_C18/1-Raw_Files/Batch_4_250515//MSMS"
  
} else if (machine == 'HILIC') {
  
  path <- sprintf('E:/Projects/CLU0120_MEC_HILIC/1-Raw_Files/%s/', endpath)
  # "E:/Projects/CLU0120_250501_MEC_C18/1-Raw_Files/Batch_4_250515/"
  ms2path <- paste0(path, '/MSMS')
  # "E:/Projects/CLU0120_250501_MEC_C18/1-Raw_Files/Batch_4_250515//MSMS"
  
} 

# Field 7 - Instrument Method 
# 5/29 - streamline 

if (machine == 'C18') {
  
  reg_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240408_C18pos_120k_BottomPump_Channel1"
  reg_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240408_C18neg_120k_TopPump_Channel2"
  
  ms2_pfas_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240802_C18pos_BottomPump_Channel1_Water_PFAS-inc-ddMS2"
  ms2_pfas_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240802_C18neg_TopPump_Channel2_Water_PFAS-inc-ddMS2"
    
  ms2_nonpfas_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18pos_BottomPump_Channel1_Water+PP-ddMS2"
  ms2_nonpfas_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18neg_TopPump_Channel2_Water+PP-ddMS2"
  
  ms2_pool_pos <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18pos_BottomPump_Channel1_Water-ddMS2"
  ms2_pool_neg <- "C:\\Xcalibur\\methods\\Aria_C18_Methods\\240801_C18neg_TopPump_Channel2_Water-ddMS2"
  
  
} else if (machine == 'HILIC') {
  
  reg_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240611_HILICpos_120k_BottomPump_Channel1"
  reg_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240411_HILICneg_120k_TopPump_Channel2"
  
  ms2_pfas_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240802_HILICpos_BottomPump_Channel1_Water_PFAS-inc-ddMS2"
  ms2_pfas_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240802_HILICneg_TopPump_Channel2_Water_PFAS-inc-ddMS2"
  
  ms2_nonpfas_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICpos_BottomPump_Channel1_Water+PP-ddMS2"
  ms2_nonpfas_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICneg_TopPump_Channel2_Water+PP-ddMS2"
  
  ms2_pool_pos <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICpos_BottomPump_Channel1_Water-ddMS2"
  ms2_pool_neg <- "C:\\Xcalibur\\methods\\Aria Methods\\240801_HILICneg_TopPump_Channel2_Water-ddMS2"
  
} 

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

cal_curves <- final_data[grepl("FBS", final_data$Sample_ID), ] 
cc_count <- nrow(cal_curves)

## 5/29 - find MDL by finding duplicate cal_curves 

dup_sample_ids <- cal_curves$Sample_ID[duplicated(cal_curves$Sample_ID)]
# "FBS_0.1ng-mL"

dup_rows <- cal_curves[cal_curves$Sample_ID %in% dup_sample_ids, ] # two rows that have that ID

# Remove the duplicate with the higher value in Col_order_by_plate (later in the plate)
dup_to_remove <- dup_rows[ave(dup_rows$Col_order_by_plate, dup_rows$Sample_ID, FUN = max) == dup_rows$Col_order_by_plate, ]

#Remove the duplicate from cal_curves
cal_curves <- cal_curves[!cal_curves$ID %in% dup_to_remove$ID, ] # overwrite; count = 20

#assign duplicate to mdls
mdls <- dup_to_remove # count = 1
#############################################

pools <- final_data[grepl("^HRE", final_data$Sample_ID), ] 
pool_count <- nrow(pools) # should be 20

# 5/29 - separate pools into pool 1, pool 2
pools1 <- pools[grepl("^HRE\\.p1", pools$Sample_ID), ] # should be 10
pools2 <- pools[grepl("^HRE\\.p2", pools$Sample_ID), ] # should be 10

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

###############
# IF SAMPLES ORGANIZED BY COLUMN - DO NOTHING
# IF SAMPLES ORGANIZED BY ROW - ORDER AS BELOW
if (exists("order_pattern") && order_pattern == "byrow") {
  study_samples <- study_samples[order(as.numeric(study_samples$Row_order_by_plate)), ]
}

# Extra - count totals for next part and to check
total_samples <- cc_count + water_count + amap_count + nist_count + study_sample_count

if(total_count == total_samples) {
  print("All qaqcs and samples were identified!")
} else {
  print(paste("Some samples were not identified. rows of dataframe =", total_count, "and sample types added together = ", total_samples))
}
# ^ for mismatch, print more info....?

### GO TO EITHER c18_lines.R or hilic_lines.R --->


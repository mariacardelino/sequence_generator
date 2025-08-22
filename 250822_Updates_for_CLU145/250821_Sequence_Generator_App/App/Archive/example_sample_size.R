# Claude help with sample size 

# Dynamic sample batching logic based on total sample count
total_samples <- nrow(study_samples)  # or however you determine total sample count
half_samples <- total_samples / 2

# Determine batching strategy based on sample count
if (half_samples <= 60) {
  # Single section: all samples in first section only
  section1_samples <- half_samples
  section2_samples <- 0
  use_two_sections <- FALSE
} else if (half_samples <= 80) {
  # Two sections: split evenly between sections
  section1_samples <- half_samples / 2
  section2_samples <- total_samples - section1_samples
  use_two_sections <- TRUE
} else {
  # More than 80 samples: use original logic (80 per section)
  section1_samples <- 80
  section2_samples <- 80
  use_two_sections <- TRUE
}

###############################################################################################
# START WRITING WITH DYNAMIC LOGIC
###############################################################################################
run_order <- 1

# Headers (same as before)
writeLines("Bracket Type=4,,,,,,,,,,,", file_conn)
writeLines("Sample Type,File Name,Sample ID,Comment,L2 Client,Path,Instrument Method,Position,Inj Vol,L1 Study,L3 Laboratory,Sample Name", file_conn)

# Initial QC samples (same as before)
# 1 instrument blank
writeLines(modify(first_blank_line_pos, run_order), file_conn)
writeLines(modify(first_blank_line_neg, run_order), file_conn)
run_order <- run_order + 1

# 1 water blank
writeLines(modify(water_lines_pos[1], run_order), file_conn)
writeLines(modify(water_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

# 1 NIST (if used)
if (use_nist) {  # assuming you have a flag for this
  writeLines(modify(nist_lines_pos[1], run_order), file_conn)
  writeLines(modify(nist_lines_neg[1], run_order), file_conn)
  run_order <- run_order + 1
}

# Pool samples before first section
writeLines(modify(pool1_lines_pos[1], run_order), file_conn)
writeLines(modify(pool1_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[1], run_order), file_conn)
writeLines(modify(pool2_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

# 3 PFAS standards
for (pfas_idx in 1:3) {
  writeLines(modify(pfas_std_lines_pos[pfas_idx], run_order), file_conn)
  writeLines(modify(pfas_std_lines_neg[pfas_idx], run_order), file_conn)
  run_order <- run_order + 1
}

###############################################################################################
# SECTION 1: First batch of study samples
###############################################################################################

sample_counter <- 1
for (i in 1:section1_samples) {
  writeLines(modify(study_sample_lines_pos[sample_counter], run_order), file_conn)
  writeLines(modify(study_sample_lines_neg[sample_counter], run_order), file_conn)
  
  # MSMS logic (same as your original)
  if (study_samples$pfms[sample_counter] == TRUE) {
    pos_index <- which(pfas_ms2_positions == study_samples$pos384[sample_counter])
    writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
    writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
  }
  
  if (study_samples$nonpfms[sample_counter] == TRUE) {
    pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[sample_counter])
    writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
    writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
  }
  
  run_order <- run_order + 1
  sample_counter <- sample_counter + 1
}

# Pool samples after first section
writeLines(modify(pool1_lines_pos[2], run_order), file_conn)
writeLines(modify(pool1_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

writeLines(modify(pool2_lines_pos[2], run_order), file_conn)
writeLines(modify(pool2_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

###############################################################################################
# MIDDLE SECTION: Only if using two sections
###############################################################################################

if (use_two_sections) {
  # 3 PFAS standards
  for (pfas_idx in 1:3) {
    writeLines(modify(pfas_std_lines_pos[pfas_idx], run_order), file_conn)
    writeLines(modify(pfas_std_lines_neg[pfas_idx], run_order), file_conn)
    run_order <- run_order + 1
  }
  
  # 1 blank before cal curve
  writeLines(modify(blank_line_pos, run_order), file_conn)
  writeLines(modify(blank_line_neg, run_order), file_conn)
  run_order <- run_order + 1
  
  # CAL CURVE (1-8)
  for (cal_idx in 1:8) {
    writeLines(modify(cal_curve_lines_pos[cal_idx], run_order), file_conn)
    writeLines(modify(cal_curve_lines_neg[cal_idx], run_order), file_conn)
    run_order <- run_order + 1
  }
  
  # 1 blank after cal curve
  writeLines(modify(blank_line_pos, run_order), file_conn)
  writeLines(modify(blank_line_neg, run_order), file_conn)
  run_order <- run_order + 1
  
  # Pool samples before second section
  writeLines(modify(pool1_lines_pos[3], run_order), file_conn)
  writeLines(modify(pool1_lines_neg[3], run_order), file_conn)
  run_order <- run_order + 1
  
  writeLines(modify(pool2_lines_pos[3], run_order), file_conn)
  writeLines(modify(pool2_lines_neg[3], run_order), file_conn)
  run_order <- run_order + 1
  
  ###############################################################################################
  # SECTION 2: Second batch of study samples (only if using two sections)
  ###############################################################################################
  
  for (i in 1:section2_samples) {
    writeLines(modify(study_sample_lines_pos[sample_counter], run_order), file_conn)
    writeLines(modify(study_sample_lines_neg[sample_counter], run_order), file_conn)
    
    # MSMS logic
    if (study_samples$pfms[sample_counter] == TRUE) {
      pos_index <- which(pfas_ms2_positions == study_samples$pos384[sample_counter])
      writeLines(modify(pfas_msms_lines_pos[pos_index], run_order, msms_type = "PFAS"), file_conn)
      writeLines(modify(pfas_msms_lines_neg[pos_index], run_order, msms_type = "PFAS"), file_conn)
    }
    
    if (study_samples$nonpfms[sample_counter] == TRUE) {
      pos_index <- which(nonpfas_ms2_positions == study_samples$pos384[sample_counter])
      writeLines(modify(nonpfas_msms_lines_pos[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
      writeLines(modify(nonpfas_msms_lines_neg[pos_index], run_order, msms_type = "nonPFAS"), file_conn)
    }
    
    run_order <- run_order + 1
    sample_counter <- sample_counter + 1
  }
  
  # Pool samples after second section
  writeLines(modify(pool1_lines_pos[4], run_order), file_conn)
  writeLines(modify(pool1_lines_neg[4], run_order), file_conn)
  run_order <- run_order + 1
  
  writeLines(modify(pool2_lines_pos[4], run_order), file_conn)
  writeLines(modify(pool2_lines_neg[4], run_order), file_conn)
  run_order <- run_order + 1
}

###############################################################################################
# FINAL QC SAMPLES
###############################################################################################

# 3 PFAS standards at the end
for (pfas_idx in 1:3) {
  writeLines(modify(pfas_std_lines_pos[pfas_idx], run_order), file_conn)
  writeLines(modify(pfas_std_lines_neg[pfas_idx], run_order), file_conn)
  run_order <- run_order + 1
}

# 1 AMAP
writeLines(modify(amap_lines_pos[1], run_order), file_conn)
writeLines(modify(amap_lines_neg[1], run_order), file_conn)
run_order <- run_order + 1

# 1 water (2/2)
writeLines(modify(water_lines_pos[2], run_order), file_conn)
writeLines(modify(water_lines_neg[2], run_order), file_conn)
run_order <- run_order + 1

# Final blank
writeLines(modify(final_blank_line_pos, run_order), file_conn)
writeLines(modify(final_blank_line_neg, run_order), file_conn)

# Close the file connection
close(file_conn)
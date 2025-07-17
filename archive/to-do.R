
# To - DO:

## Inventory check printout happens even when no samples.
## Recognize which pools are which (instead of assume first 10 are pool 10, second 10 are pool 2).
## Save to separate folder?
## Automatically identify halfway point? Number of samples? 

## A way to search what 384 position from its plate and position? (For mistakes)
  ## just add search bar to table... different type of table. easy conversion


## SHINY input tab -------------------------- 
# Select tab for path up until 1-Raw_Files 
# 'Admin' tab?

# 5/23 

## Fixed error in HILIC files - MDL method path, NIST name 
## Moved read_csv_helper function to sever'
## FIXED the T/F on the Testcopy sheet - up to date.

# 5/20 completed 

# Fix HILIC injection volumes - both are 5 uL

# 5/16 COMPLETED ####################################################
# run_parameters -----------------------
# allow c18 and hilic to use the same variable 'path' DONE
# tried HILIC:

## SHINY input tab -------------------------- 
# Rack numbers for input in l2 DONE
# Change so the filename is auomatically generated. DONE
# If the file exists, add a number so it doesn't overwrite the old.DONE


## OTHER -----------
# Make it automatically sort based on if the inventory is by row or column DONE
# make it automatically see how many amap there are and add them. LATER NOT NOW
# FIX RUN ORDER FOR BLANKS!! DONE
# Fix the rack split in half. DONE
# Make sure the new path date is correct for column F. DONE

######################## OLD #############################
### 5/15 to do: 

# Allow users to select the parent folder path for the study? Or just fix it. The date is wrong

# Fixed AMAP --> AM-S . Still need to check the QAQC identifier - order looks weird.

# Tab 4 data input:
# Make incorrect entering impossible (check formatting)
# Autofill - for things like date, study, and Doug's format for file.

# Add an automatic decision to order by row (like 120) or by column.


#################################################

# 5/13 whole thing works for C18. THROUGH THE APP!! WOO HOO!!

# Next: clean up and separate shiny scripts.

# Next: Allow user to switch order of sample in study_samples to be ordered by row
# so they are injected together. (or do it automatically by checking inventory?)

# Next: Pull data from actual up-to-date study inventory.

# Create running document for things we have already run so we can say
# You've already analyzed this.

#############################################################

# 5/12 1,2,3 complete with plate images
# working on sequence order
# need to get hilic method paths and orders (same?)
# Tonight: 
# instead of using qaqc.Rdata, use plate_loading_order?
# OR... make new sheet that updates with plate loading order 
# and keeps track of what has been run?

# 5/10 pulling micronic data, assigning sample type, pulling QAQC sample names
# rack 13 - sample plate 1 (80 samples)
# rack 14 - sample plate 2 (80 samples)
# rack 15 - QAQC plate (48 samples)

# SUCCESS! But...
# ORDER IS BY ROW , not column. Fix later.
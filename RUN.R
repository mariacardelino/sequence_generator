####################################################################        ######################################                                                                ######################################
                                                                            #                                    #                                                                #                                    #
# Micronic Plate reader .csv --> Sequence List Generator Script             #      CLICK 'RUN APP' ^^^           #                                                                #      CLICK 'RUN APP' ^^^           #
# Hands-free ARPA-H sample tracking                                         #      CLICK 'RUN APP' ^^^           #                                                                #      CLICK 'RUN APP' ^^^           #
# Click 'Run App' in top right corner >>>                                   #                                    #                                                                #                                    #
                                                                            ######################################                                                                ######################################
#################################################################### 

# List of required packages
required_packages <- c(
  "openxlsx", "shinyjs", "tools",
  "shiny", "readr", "dplyr", 
  "plotly", "readxl", "shinyFiles"
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



# Change the wd to the folder the app is in - MORE ROBUST changed 5/30
v <- getwd()
setwd(v)

# Change the wd to the folder the app is in
#setwd("R:/diwalke/LC/Run_Lists/CLU0120 (MEC)/Sequence_Generator")

# 5/21 version on Maria Laptop:
#setwd("C:/Users/mcard/OneDrive - Emory/Doug/ARPAH_R_FOLDER/Sequence_List_App/v0523/Sequence_Generator")

# CHECK: Change Micronic plate reader to automatically save .csvs to this folder (R drive)
# R:/diwalke/LC/Run_Lists/Plate_Scans

load("App/Mapping/map.Rdata")  # loads 'df' (96-positions and orders) and 'map' (384-positions)

inventory_file_path <<- "TestCopy_250409_CLU0120_Plate_Loading_Order_Updated-Again.xlsx"

qaqcs <- read_excel(inventory_file_path, sheet = "QAQCs")
samples <- read_excel(inventory_file_path, sheet = "Samples")

# remove extra cols
qaqcs <- qaqcs %>% 
  select(-QAQC_RN)

# As character 
samples$RackNum <- as.character(samples$RackNum)

## JOIN QAQCS and SAMPLE DATA TO CLU0120_inventory
inventory <- bind_rows(qaqcs, samples) 
# columns: Matrix_ID Sample_ID Position RackNum Sample_Type

# run the app or press the Run App button ^^
runApp("App")

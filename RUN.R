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
  "plotly", "readxl", "shinyFiles", "stringr"
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
# setwd(v)

# If need to manually set:
# setwd("R:/diwalke/LC/Run_Lists/Sequence_Generator")

load("App/Mapping/map.Rdata")  # loads 'df' (96-positions and orders) and 'map' (384-positions)

# Old combined inventory: changed 7/21
#inventory_file_path <<- "TestCopy_250409_CLU0120_Plate_Loading_Order_Updated-Again.xlsx"

# Inventory
q_filepath <- "QAQCS.xlsx"
s_filepath <- "Samples.xlsx"

qaqcs <- read_excel(q_filepath, sheet = "QAQCs")
samples <- read_excel(s_filepath, sheet = "Samples")

# altering?

qaqc_inventory <- qaqcs
sample_inventory <- samples

# run the app or press the Run App button ^^
runApp("App")

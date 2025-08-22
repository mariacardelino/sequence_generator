####################################################################        ######################################                                                                ######################################
                                                                            #                                    #                                                                #                                    #
# Micronic Plate reader .csv --> Sequence List Generator Script             #      CLICK 'RUN APP' ^^^           #                                                                #      CLICK 'RUN APP' ^^^           #
# Hands-free ARPA-H sample tracking                                         #      CLICK 'RUN APP' ^^^           #                                                                #      CLICK 'RUN APP' ^^^           #
# Click 'Run App' in top right corner >>>                                   #                                    #                                                                #                                    #
                                                                            ######################################                                                                ######################################
####################################################################

#### VERSION info ##############################

## Study affiliation: CLU0145
## Creation date: 250821

#write_list_Batch1 : works with 80, 80 samples per rack
#write_list_Batch2: works with 80 samples in rack 3 and 10 samples in rack 2 (half sequence)

## NOTES: for 8/25/25:

# To change Batch 1/Batch 2" comment out at line 16 and 1416 of server.R ######################

# Notes for Batch 2: COULD REMOVE 3 pool - only need 7 each!!!!
  # not very important: 20 samples discrepancy between rows of final dataframe (124) and samples added together (104) - pools missing?
  # changed MSMS (PFAS and nonPFAS) locations so there would be 6 each in Batch 2

# The automatic order detection in sample inventory is wrong 
  # (in the inventory it is organized column-wise, but the app says it appears to be organized by row)

################################################

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

load("App/Mapping/map.Rdata")  # loads 'df' (96-positions and orders) and 'map' (384-positions)

# Inventory
q_filepath <- "QAQCS.xlsx"
s_filepath <- "Samples.xlsx"

qaqcs <- read_excel(q_filepath, sheet = "QAQCs", col_types = "text")
qaqcs$Analyzed <- as.logical(qaqcs$Analyzed)

samples <- read_excel(s_filepath, sheet = "Samples", col_types = "text")
samples$Analyzed <- as.logical(samples$Analyzed)
# altering?

qaqc_inventory <- qaqcs
sample_inventory <- samples

# run the app or press the Run App button ^^
runApp("App")

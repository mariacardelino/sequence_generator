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

# Old combined inventory: changed 7/21
#inventory_file_path <<- "TestCopy_250409_CLU0120_Plate_Loading_Order_Updated-Again.xlsx"

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

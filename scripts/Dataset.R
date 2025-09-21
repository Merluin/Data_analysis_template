# Header ----
#  Programmer:  QUETTIER THOMAS
#  Experiment: 
#  Description: take original data and built a unique dataset
#  Update:      20/09/25

# Clearing workspace ----
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies ----
devtools::load_all() # Load necessary functions and packages

folder_dir <- "original_data/psychopy"
 
# Data loading ----
datasetname <-  "psychopy_dataset"
dataset_concatenation(folder_dir,datasetname) # aggregates all .csv files use finction in R folder
load(paste0("data/",datasetname,".RData") )

# Count participants
n <- length(unique(dataset$participant)) # Calculate the number of unique participants

# mutate dataset ----
data <- dataset %>%
  mutate(id = participant) # here applay modification, filter to the original dataset 

# Save the data in .RData format ----
save(dataset, data, file = paste0("data/",datasetname,".RData"))

# END ----
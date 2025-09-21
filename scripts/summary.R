# ---- HEADER ----
#  Experiment:  Sara Borgomaneri emotion
#  Programmer:  QUETTIER THOMAS
#  Date:        09/09/2025
#  Update:      09/09/2025
#  Affiliation: UNIBO 

## ---- Setting ----

# Clear the existing workspace to avoid conflicts
rm(list = ls())  

# Load necessary functions and packages (assumes this script is part of a package or project)
devtools::load_all()  

## ---- Data Loading ----
load("data/full_dataset.RData") 

# Analysis ----
## Summarise Data Exp 1----
dataset <- dataset # summarise 

# Save ----
## Export XlS ----
wb <- createWorkbook()
# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "foglio1")
# Write data to the corresponding sheet
writeData(wb, sheet = "foglio1", dataset)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "foglio2")
# Write data to the corresponding sheet
writeData(wb, sheet = "foglio2", data)

# Save the workbook
saveWorkbook(wb, "data/data.xlsx", overwrite = TRUE)

# END ----
# File or Package Name: TREHU1_merge_XLS.R
# Title: Merging 2 Excel files dataset
# Description: Script for merging 2 Excel files from ./data 
#             where one come RESULTAT and the second is a tibble,  
#             with the final dataframe in ./ouput
# Version: 1.0.0
# Author: Patrice Tardif
# Maintainer: Patrice Tardif
# License: MIT
# Date: 2019-11-14




# load libraries
library(tidyverse)
library(readxl)



## Paramaters for data location and output in the project
#   Consider that the fNameIn1 - FDnbPint.XLS - come from RESULTATS app and
#   could be replace with a view or direct SQL request at this line.
PATH_XL <-  "./data"
PATH_OUT <- "./output"
fNameIn1 <- file.path(PATH_XL, "FDnbPint.XLS")
fNameIn2 <- file.path(PATH_XL, "2019_Points_intercepts.xlsx")
if (!dir.exists(PATH_OUT)) dir.create(PATH_OUT)
fNameOut <- file.path(PATH_OUT, "2019_Points_intercepts_merged.csv")
  


## Load, Extract, Tansform  (ETL) the XL files

# Load fNameIn1 and Extract only 3 cols of df/TODO SQL request replacement
df_FD <- read_excel(fNameIn1,"FDnbPint.XLS") %>% 
  as.data.frame(.) %>% 
  '['(., , c("ID__PLACET" ,"ESPECE_VEG","NOMBRE")) 

# Test that NA occur at the same place in VEG and NOMBRE
if (any(xor(is.na(df_FD$ESPECE_VEG), is.na(df_FD$NOMBRE)))) {
  stop("Non-matching NA inside FDnbPint.XLS between ESPECE_VEG and NOMBRE")
} 
names(df_FD) <- c("ID", "SP", "NB") # Rename df cols

# Recode by REGEX old ID__PLACET in no_placette for matching key
df_FD$no_placette <- 
  str_extract(df_FD$ID, "^[:alpha:]{1}[:digit:]{3}") 

# Display non-matching Recode and their numbers
if (any(is.na(df_FD$no_placette ))) {
  cat("\n### --- \n\n")
  cat(sprintf("%s\n",unique(df_FD$ID[is.na(df_FD$no_placette)])))
  cat(sprintf("\n###  %d NON-Recoding [NA] occurences (lines) in %s \n", 
              sum(is.na(df_FD$no_placette )), fNameIn1))
} 

# Remove NA lines
df_FD <-  drop_na(df_FD, no_placette, NB, SP)

# Load fNameIn2 and split in 2 parts (species and non-species variables)
df_PI <- read_excel(fNameIn2,"points_intercept") %>% 
  as.data.frame(.)
ls_col_ext <- c("equipe", "annee_feu", "secteur", "Vaccinium borealis", "LfhB")
df_PI_core <- df_PI[, !(names(df_PI) %in% ls_col_ext)] # species 
df_PI_ext <- df_PI[, names(df_PI) %in% c("no_placette", ls_col_ext)] #non-sp.

# Transform fNameIn1 format in fNameIn2 table
df_PI_FD <- spread(df_FD, key = "SP", value = "NB") %>% 
  subset(.,select = -ID) #drop ID column

# Merge non-species variables and empty species to the crossjoin result
df_Merged <- merge(df_PI_ext, df_PI_FD, by = "no_placette", all = T) %>%
  merge(., df_PI_core[,c("no_placette", names(df_PI_core)[which(!(names(df_PI_core) %in% names(df_PI_FD)))])]
        , by = "no_placette", all = T)



## Export also the dataframe in CSV format

write.csv(df_Merged, fNameOut, na = "", row.names = F)



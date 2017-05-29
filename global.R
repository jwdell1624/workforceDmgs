suppressPackageStartupMessages({
  library(shiny)        # web framework
  library(readr)        # fast I/O
  library(plotly)       # interactive ploting
  library(ggplot2)      # data visualisation
  library(dplyr)        # data wrangling
  library(tidyr)        # tidy data
})

# Import Workforce Analytics team functions --------------------------------------------------------

# Group classifications
source("/proj/workforce/www/scriptsMiscAdhocs/waFunctions/clssnGrpFun.R")

# Sum HC and percentage of groups
source("/proj/workforce/www/scriptsMiscAdhocs/waFunctions/scaleHCFun.R")

# Remove N/As and convert binary to yes/no response
source("/proj/workforce/www/scriptsMiscAdhocs/waFunctions/naTreatmentFun.R")

# Remove N/As and convert binary to yes/no response
source("/proj/workforce/www/scriptsMiscAdhocs/waFunctions/colOrderFun.R")

# Enable Bookmarking -------------------------------------------------------------------------------

enableBookmarking(store = "url")

# Load data ----------------------------------------------------------------------------------------

# Set path for data files
dataPath <- "/proj/workforce/data/shinyApps/"

# Read snapshot data
df <- read_csv(paste0(dataPath, "workforceDmgs/wddDmgs.csv"))

# DATA CLEANSE -------------------------------------------------------------------------------------

# remove CSA Externals
df <- subset(df, df$Cost_Centre_Text != "CSA External Staff")

# Remove NAs and reformat binary answers to Yes/No 
na.cols <- c("NESB1_HC", "NESB2_HC", "Disability_HC", "Indigenous_HC", "MDP_Completion_Percent"
             , "Mobility_Indicator", "OOM_Indicator", "Manager_Indicator")
df      <- data.frame(df[, !names(df) %in% na.cols], lapply(df[na.cols], function(x) na.treat(x)))

# Temporary fix for function problem with F2F_Count, eLRN_Count, External_Count and External_Cost
df$F2F_Count <- ifelse(is.na(df$F2F_Count), 0, df$F2F_Count)
df$eLRN_Count <- ifelse(is.na(df$eLRN_Count), 0, df$eLRN_Count)
df$External_Count <- ifelse(is.na(df$External_Count), 0, df$External_Count)
df$External_Cost <- ifelse(is.na(df$External_Cost), 0, df$External_Cost)

# NESB_Sum equal to "Yes" if NESB1_HC OR NESB2_HC equal "Yes"
df$NESB_Sum <- ifelse(df$NESB1_HC == "Yes", "Yes", ifelse(df$NESB2_HC == "Yes", "Yes", "No"))

# Add grouped classification column
df <- clssnGrpFun(df)

# Reformat as factor for Tenure, Classification and Age Range plot order
df$Actual_Classification <- clssnOrder(df)
df$clssnCat              <- clssnCatOrder(df)
df$ATO_Tenure_Range      <- tnrRngOrder(df)
df$Age_Range_5yr         <- ageRngOrder(df)
suppressPackageStartupMessages({
  library(shiny)        # R web framework
  library(readr)        # fast I/O
  library(plotly)       # plotly.js for R
  library(dplyr)        # data wrangling
  library(tidyr)        # tidy data
})

# SOURCE FUNCTIONS ---------------------------------------------------------------------------------

# Group classifications
source("functions/clssnGrpFun.R")

# Remove N/As and convert binary to yes/no response
source("functions/naTreatmentFun.R")

# Re-order factors
source("functions/colOrderFun.R")

# BOOKMARKING --------------------------------------------------------------------------------------

enableBookmarking(store = "url")

# LOAD DATA ----------------------------------------------------------------------------------------

# Read snapshot data
df <- read_csv("data/random_df.csv")

# DATA CLEANSE -------------------------------------------------------------------------------------

# Remove CSA Externals
df <- subset(df, df$Cost_Centre_Text != "CSA External Staff")

# Remove NAs and reformat binary answers to Yes/No 
na.cols <- c("NESB1_HC", "NESB2_HC", "Disability_HC", "Indigenous_HC", "MDP_Completion_Percent"
             , "Mobility_Indicator", "OOM_Indicator", "Manager_Indicator")
df      <- data.frame(df[, !names(df) %in% na.cols], lapply(df[na.cols], function(x) na.treat(x)))

# Temp. fix for function problem with F2F_Count, eLRN_Count, External_Count and External_Cost
df$F2F_Count      <- ifelse(is.na(df$F2F_Count), 0, df$F2F_Count)
df$eLRN_Count     <- ifelse(is.na(df$eLRN_Count), 0, df$eLRN_Count)
df$External_Count <- ifelse(is.na(df$External_Count), 0, df$External_Count)
df$External_Cost  <- ifelse(is.na(df$External_Cost), 0, df$External_Cost)

# NESB_Sum equal to "Yes" if NESB1_HC OR NESB2_HC equal "Yes"
df$NESB_Sum <- ifelse(df$NESB1_HC == "Yes", "Yes", ifelse(df$NESB2_HC == "Yes", "Yes", "No"))

# Add grouped classification column
df <- clssnGrpFun(df)

# Reformat as factor for Tenure, Classification and Age Range plot order
df$Actual_Classification <- clssnOrder(df)
df$clssnCat              <- clssnCatOrder(df)
df$ATO_Tenure_Range      <- tnrRngOrder(df)
df$Age_Range_5yr         <- ageRngOrder(df)
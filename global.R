# TODO wrap suppress messages, test suppressPackageStartupMessages() 
# TODO add brief comment next to each library loaded
suppressMessages(library(shinydashboard))
suppressMessages(library(readr))
suppressMessages(library(plotly))       # interactive ploting
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))        # data wrangling
suppressMessages(library(tidyr))

# Enable Bookmarking
enableBookmarking(store = "url")

# Set path for data files
dataPath <- "/proj/workforce/data/shinyApps/"

# Read snapshot data
df <- read_csv(paste0(dataPath, "workforceDmgs/wddDmgs.csv"))

# DATA CLEANSE -------------------------------------------------------------------------------------

# remove CSA Externals
df <- subset(df, df$Cost_Centre_Text != "CSA External Staff")

# TODO - refactor (DRY)
# remove na's 
df$NESB1_HC               <- ifelse(is.na(df$NESB1_HC), 0, 1)
df$NESB2_HC               <- ifelse(is.na(df$NESB2_HC), 0, 1)
df$Disability_HC          <- ifelse(is.na(df$Disability_HC), 0, 1)
df$Indigenous_HC          <- ifelse(is.na(df$Indigenous_HC), 0, 1)
df$MDP_Completion_Percent <- ifelse(is.na(df$MDP_Completion_Percent), 0, df$MDP_Completion_Percent)
df$F2F_Count              <- ifelse(is.na(df$F2F_Count), 0, df$F2F_Count)
df$eLRN_Count             <- ifelse(is.na(df$eLRN_Count), 0, df$eLRN_Count)
df$External_Count         <- ifelse(is.na(df$External_Count), 0, df$External_Count)
df$External_Cost          <- ifelse(is.na(df$External_Cost), 0, df$External_Cost)

# TODO - see function used in bivariateDmgs to adhere to DRY across the next 9 lines
# sum nesb1 and nesb2 
df$NESB_Sum <- df$NESB1_HC + df$NESB2_HC

# re-format to Yes/No for pie charts
df$NESB_Sum           <- ifelse(df$NESB_Sum == 0, "No", "Yes")
df$Disability_HC      <- ifelse(df$Disability_HC == 0, "No", "Yes")
df$Indigenous_HC      <- ifelse(df$Indigenous_HC == 0, "No", "Yes")
df$Mobility_Indicator <- ifelse(df$Mobility_Indicator == "N", "No", "Yes")
df$OOM_Indicator      <- ifelse(df$OOM_Indicator == "N", "No", "Yes")
df$Manager_Indicator  <- ifelse(df$Manager_Indicator == "N", "No", "Yes")

# TODO - create function to handle this as repeated across WA code base
# re-format as factor for Tenure, Classification and Age Range plot order
df$Actual_Classification <- factor(df$Actual_Classification
                                   , levels = c("Not assigned", "EXT", "CAD", "APS1", "APS2", 
                                                "APS3", "APS4", "APS5", "APS6", "EL1", "EL21", 
                                                "EL22", "SES1", "SES2"))
df$ATO_Tenure_Range      <- factor(df$ATO_Tenure_Range
                                   , levels = c("Not assigned", "< 5","5 - 9", "10 - 14","15 - 19"
                                                ,"20 - 24", "25 - 29","30 - 34","35 - 39","40 - 44"
                                                ,"45 - 49",">= 50"))
df$Age_Range_5yr         <- factor(df$Age_Range_5yr
                                , levels = c("< 20","20 - 24","25 - 29","30 - 34","35 - 39"
                                             , "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64"
                                             , "65 - 69", ">= 70"))
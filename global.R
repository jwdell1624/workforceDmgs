suppressMessages(library(shinydashboard))
suppressMessages(library(readr))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

# Read data
wddDmgs <- read_csv("data/wddDmgs.csv")

#__________________________________________________________________________________________________#

# Data cleanse ####

# remove CSA Externals
wddDmgs <- subset(wddDmgs, wddDmgs$Cost_Centre_Text != "CSA External Staff")

# remove na's 
wddDmgs$NESB1_HC               <- ifelse(is.na(wddDmgs$NESB1_HC), 0, 1)
wddDmgs$NESB2_HC               <- ifelse(is.na(wddDmgs$NESB2_HC), 0, 1)
wddDmgs$Disability_HC          <- ifelse(is.na(wddDmgs$Disability_HC), 0, 1)
wddDmgs$Indigenous_HC          <- ifelse(is.na(wddDmgs$Indigenous_HC), 0, 1)
wddDmgs$MDP_Completion_Percent <- ifelse(is.na(wddDmgs$MDP_Completion_Percent), 0, wddDmgs$MDP_Completion_Percent)
wddDmgs$F2F_Count              <- ifelse(is.na(wddDmgs$F2F_Count), 0, wddDmgs$F2F_Count)
wddDmgs$eLRN_Count             <- ifelse(is.na(wddDmgs$eLRN_Count), 0, wddDmgs$eLRN_Count)
wddDmgs$External_Count         <- ifelse(is.na(wddDmgs$External_Count), 0, wddDmgs$External_Count)
wddDmgs$External_Cost          <- ifelse(is.na(wddDmgs$External_Cost), 0, wddDmgs$External_Cost)

# sum nesb1 and nesb2 
wddDmgs$NESB_Sum <- wddDmgs$NESB1_HC + wddDmgs$NESB2_HC

# re-format to Yes/No for pie charts
wddDmgs$NESB_Sum           <- ifelse(wddDmgs$NESB_Sum == 0, "No", "Yes")
wddDmgs$Disability_HC      <- ifelse(wddDmgs$Disability_HC == 0, "No", "Yes")
wddDmgs$Indigenous_HC      <- ifelse(wddDmgs$Indigenous_HC == 0, "No", "Yes")
wddDmgs$Mobility_Indicator <- ifelse(wddDmgs$Mobility_Indicator == "N", "No", "Yes")
wddDmgs$OOM_Indicator      <- ifelse(wddDmgs$OOM_Indicator == "N", "No", "Yes")
wddDmgs$Manager_Indicator  <- ifelse(wddDmgs$Manager_Indicator == "N", "No", "Yes")

# re-format as factor for Tenure, Classification and Age Range plot order
wddDmgs$Actual_Classification <- factor(wddDmgs$Actual_Classification
                                        , levels = c("Not assigned","EXT","CAD","APS1","APS2","APS3"
                                                     ,"APS4","APS5","APS6","EL1","EL21","EL22","SES1"
                                                     ,"SES2","SES3"))
wddDmgs$ATO_Tenure_Range <- factor(wddDmgs$ATO_Tenure_Range
                                   , levels = c("Not assigned", "< 5","5 - 9", "10 - 14","15 - 19"
                                                ,"20 - 24", "25 - 29","30 - 34","35 - 39","40 - 44"
                                                ,"45 - 49",">= 50"))
wddDmgs$Age_Range_5yr <- factor(wddDmgs$Age_Range_5yr
                                , levels = c("< 20","20 - 24","25 - 29","30 - 34","35 - 39"
                                             , "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64"
                                             , "65 - 69", ">= 70"))

#__________________________________________________________________________________________________#

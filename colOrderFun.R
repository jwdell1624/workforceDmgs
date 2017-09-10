# Description---------------------------------------------------------------------------------------
# Reformat columns as factors with explicit ordering

# Usage---------------------------------------------------------------------------------------------
# bslUI(df)
# bslUI(df, df_col)

# Arguments-----------------------------------------------------------------------------------------
# df              a dataframe

# df_col          the name of the column to be ordered, note that default column names are set

# Functions ----------------------------------------------------------------------------------------

# Set ordering for classification
clssnOrder <- function(df, df_col = "Actual_Classification"){
    factor(df[[df_col]]
           , levels = c("Not assigned", "EXT", "CAD", "APS1", "APS2",
                        "APS3", "APS4", "APS5", "APS6", "EL1", "EL21",
                        "EL22", "SES1", "SES2"))
}

# Set ordering for categorised classification
clssnCatOrder <- function(df, df_col = "clssnCat"){
    factor(df[[df_col]]
           , levels = c("Not assigned", "General Employee"
                        , "Executive Level", "Senior Executive"))
}

# Set ordering for tenure range
tnrRngOrder <- function(df, df_col = "ATO_Tenure_Range"){
    factor(df[[df_col]]
           , levels = c("Not assigned", "< 5","5 - 9", "10 - 14","15 - 19"
                        ,"20 - 24", "25 - 29","30 - 34","35 - 39","40 - 44"
                        ,"45 - 49",">= 50"))
}

# Set ordering for age range
ageRngOrder <- function(df, df_col = "Age_Range_5yr"){
    factor(df[[df_col]]
           , levels = c("< 20","20 - 24","25 - 29","30 - 34","35 - 39"
                        , "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64"
                        , "65 - 69", ">= 70"))
}
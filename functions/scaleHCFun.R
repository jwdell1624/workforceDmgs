# Description---------------------------------------------------------------------------------------
# Function to group df by specified columns and output a HC and percentage of each grouping

# Usage---------------------------------------------------------------------------------------------
# bslUI(df, "Subplan")
# bslUI(df, c("Subplan", "BSL"))

# Arguments-----------------------------------------------------------------------------------------
# df              a dataframe
# columns         one or more columns from df to group on
# selView         a shiny radio input with options "Headcount" and "Percentage"

suppressMessages(library(dplyr))

scaleHC <- function(df, columns=c(), selView=""){
    
    if (selView == "Headcount"){
        df %>%
            select_(.dots = columns) %>%
            group_by_(.dots = columns) %>%
            summarise(Measure = n()) -> x
        return(x) # Return Measure = HC
    } else if (selView == "Percentage") {
        df %>%
            select_(.dots = columns) %>%
            group_by_(.dots = columns) %>%
            summarise(HC = n()) %>%
            ungroup() %>%
            mutate(Measure = round(HC/sum(HC)*100, 2)) -> x
        return(x) # Return Measure = Percentage
    } else {
        df %>%
            select_(.dots = columns) %>%
            group_by_(.dots = columns) %>%
            summarise(HC = n()) %>%
            ungroup() %>%
            mutate(Percent = round(HC/sum(HC)*100, 2)) -> x
        return(x) # Return HC and Percent with no Measure column
    }
}
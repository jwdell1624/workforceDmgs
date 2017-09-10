# Function to group staff based on classification range
# df is a dataframe with a employee classification column 
clssnGrpFun <- function(df, clssn="Actual_Classification"){
  
  # Classificaton groupings
  ge   <- c("APS1","APS2","APS3","APS4","APS5","APS6","GAA","CAD","EXT")
  el   <- c("EL1","EL21","EL22")
  ses  <- c("SES1","SES2","SES3")
  na   <- c("Not assigned")
  exec <- c("EL1","EL21","EL22","SES1","SES2","SES3")
  
  # Add Classification Category and Executive Staff columns
  df["clssnCat"]  <- ""
  df["execStaff"] <- FALSE
  
  # Reorder the new columns to appear after classificaton
  i <- which(colnames(df)==clssn)
  j <- which(colnames(df)=="clssnCat")
  df <- df[,c(1:i,j,j+1,(i+1):(j-1))]
  
  # Assign values to the new columns
  df$clssnCat[df$Actual_Classification %in% ge]   <- "General Employee"
  df$clssnCat[df$Actual_Classification %in% el]   <- "Executive Level"
  df$clssnCat[df$Actual_Classification %in% ses]  <- "Senior Executive"
  df$clssnCat[df$Actual_Classification %in% na]   <- "Not assigned"
  df$execStaff[df$Actual_Classification %in% exec] <- TRUE

  return(df)
}
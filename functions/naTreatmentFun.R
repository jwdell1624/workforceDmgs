# Function copied from bivariateDmgs
# TO DO - use this to replace function embedded in bivariateDmgs
# TO DO - make sure function continues to work

# Function to treat NAs
na.treat <- function(df.col, t="Yes", f="No") {
    # when column contains NAs replace with 0s and convert to Yes/No
    if (anyNA(df.col)) {
        df.col[is.na(df.col)] <- 0
        df.col <- ifelse(df.col == 1, t, ifelse(df.col == "Y", t, f))
    } else {
        # convert 1s and 0s or Ys and Ns
        df.col <- ifelse(df.col == 1, t, ifelse(df.col == "Y", t, f))
    }
}

## We have saved the tunings in individual files for a given eta and max.depth,
## in directories named by subsample and colsample_bytree. We need a function that takes
## a list of csv file names and combine them into a single dataframe

combinedTunings <- function(fns) {
  df <- read.csv(fns[1])
  for (fn in fns[-1] ) {
    df <- rbind(df, read.csv(fn))
  }
  df
}



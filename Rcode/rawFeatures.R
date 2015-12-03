## Process data for use by XGBoost.
## We will not use any preprocessing over time.
## The goal is to obtain the best model without using time series properties
## of the data. Once we have the data that gives us the best model, we can add
## new features that represent the time series properties of the data.

## We can experiment with the features and tune the model for each set of
## parameters. We need to follow an approach that makes model building simple.
## We should write an R script that provides a recipe to produce features from
## the original csvs, and saves them in Rdata. An associated R script should
## tune the model, and another should train and produce the submission. These
## files should share their name. For example,
## xxx.produce_data.R, xxx.tune_XGBoost.R, and xxx.train_XGBoost.R, is a good
## naming scheme.

## In this script we do not perform any fancy feature extraction. We have
## learnt from pervious experimentation that train and test data should be
## processed together. We were not able to identify the exact problem that
## scrambled the test data when we tried pre-processing test separately from
## train. However, since predictors are shared across train and test, a combined
## preprocessing of the two data-sets is sensible.
## ----loaddata-------------

print("load the data, and parse dates")
training <- read.csv("../data/train.csv")
testing <- read.csv("../data/test.csv")
store <- read.csv("../data/store.csv")
training$Date <- ymd(training$Date)
testing$Date <- ymd(testing$Date)


## ----useonlyopenstores---------------------------------------------------

print("Days when the sale is zero will not be used for evaluation")
print("For training, use only open stores, ")
print("and those days when they have non-zero sales.")
print("We can experiment with keeping the zero sale days as well")
print("and see the performance benefit or hit it causes")
print("Some values of Open in test data are unknown")
print("... we will replace these with 1, that is assume that the store was open")
print("... if the store was instead closed , its sale should be zero")
print("... and thus should not contribute towards evaluation")

testing$Open[is.na(testing$Open)] <- 1
training <- subset(training, Open != 0)
training <- subset(training, Sales > 0)

##----add remove cols------
print("In order to combine test and train data, ")
print("we need to have the same columns in the two data-sets")

testing$Sales <- NA
testing$Customers <- NA
training$Id <- 1:nrow(training)

##----combine test train
print("and a column that tells us which column is which")

training$tr.ts <- "tr"
testing$tr.ts <- "ts"
trts <- rbind(training, testing)


## ----merge store---------------------------------------------------------

print("We can now safely merge the store data into the sales data")
trts <- merge(trts, store, by="Store")

## ----featuredirect-------------------------------------------------------

print("Some features will be used as they are: ")
preds <- c('Store', 'CompetitionDistance',
           'CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear',
           'Promo', 'Promo2',
           'Promo2SinceWeek', 'Promo2SinceYear')
print(preds)


## ----processedFeatures---------------------------------------------------

print("Character/factor data needs to be converted to integers for XGBoost")
print("SchoolHoliday is simple, 0 or 1")
trts$SchoolHoliday <- as.numeric(trts$SchoolHoliday)
preds <- c(preds, "SchoolHoliday")
print("Though StateHoliday has more than two categories")
print("they are extremely unbalanced.")
print("most frequent being not a holiday.")
print("We will use a grouped-category representation for these,")
print("... this is another point that can be experimented with.")
print("EXPERIMENT: use independent categories for StateHoliday")
trts$StateHoliday <- as.numeric(trts$StateHoliday)
preds <- c(preds, "StateHoliday")
print("Extract date variables")
trts <- cbind(trts,
              data.frame(
                  Year = year(trts$Date),
                  Month = month(trts$Date),
                  DayOfWeek = wday(trts$Date),
                  DayOfMonth = mday(trts$Date))
              )
preds <- c(preds, "Year", "Month", "DayOfWeek", "DayOfMonth")
print("Use independent categories for store type and assortment.")
print("EXPERIMENT: use grouped categories for these variables.")
storeType.ic <- independentCategories( "StoreType", trts)
trts <- cbind(trts, storeType.ic)
preds <- c(preds, names(storeType.ic))
assortmnent.ic <- independentCategories("Assortment", trts)
trts <- cbind(trts, assortmnent.ic)
preds <- c(preds, names(assortmnent.ic))

print("Final set of predictors: ")
print(preds)
##---- extract training and testing out of trts
print("Predict log of sales ")
trts$LogSales <- log(trts$Sales)
outcomes <- c("LogSales")
print("Separate predictor data from outcome,")
print("Necessary because NAs in predictor data need to be filled in")
#trts.p <- trts[, c("Id", "tr.ts", preds)]
trts.p <- trts[, setdiff(names(trts), outcomes)]
trts.o <- trts$LogSales

print("We use the simple imputation of setting all NAs to 0")
print("EXPERIMENT: use another value than 0, such as -1")
print("EXPERIMENT: use a more elaborate imputation method")
trts.p[is.na(trts.p)] <- 0
print("Combine imputed predictor data with the outcome")
trts <- trts.p
trts$LogSales <- trts.o

print("Separate the processed training data from the processed testing data")
trp <- subset(trts, tr.ts=="tr")
tsp <- subset(trts, tr.ts=="ts")
print("Add deviation from log sas as a column to processed training data.")
print("This can be used as an outcome")
trp$DeviationLogSales <- with(trp, LogSales - mean(LogSales))

meanLogSalesByStore <- dply(trp, .(Store), summarise,
                             MeanLogSales = mean(LogSales),
                             SdLogSales = sd(LogSales))

trp <- merge(trp, meanLogSalesByStore, by="Store")
tsp <- merge(tsp, meanLogSalesByStore, by="Store")
trp$DeviationLogSalesByStore <- with(trp, LogSales - MeanLogSales)
print("save data to Rdata")
fn <- "../data/processed/data_xgb_non_time-series"
print("save data, but only the predictors to be used for modeling")
trpp <- trp[, c("LogSales", "DeviationLogSales", preds)]
tspp <- tsp[, preds]
save(trpp, file = paste(fn, "training", "Rdata", sep="."))
save(tspp, file = paste(fn, "testing", "Rdata", sep="."))
write.csv(trpp, file = paste(fn, "training", "csv", sep="."))
write.csv(tspp, file = paste(fn, "testing", "csv", sep="."))

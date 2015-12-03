print("Build simple features for linear models:   WE CANNOT ALLOW INFINITIES")
print("load the required libraries")
require(lubridate)
require(plyr)
source("../Rcode/indpCats.R")
print("load the data, and parse dates")
training <- read.csv("../data/train.csv")
testing <- read.csv("../data/test.csv")
store <- read.csv("../data/store.csv")
training$Date <- ymd(training$Date)
testing$Date <- ymd(testing$Date)


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

##----combine test train
###----add remove cols------
print("In order to combine test and train data, ")
print("we need to have the same columns in the two data-sets")

testing$Sales <- NA
testing$Customers <- NA
training$Id <- 1:nrow(training)

print("and a column that tells us which column is which")

training$tr.ts <- "tr"
testing$tr.ts <- "ts"
trts <- rbind(training, testing)
trts$tr.ts <- as.character(trts$tr.ts)


## ----merge store---------------------------------------------------------

print("We can now safely merge the store data into the sales data")
trts <- merge(trts, store, by="Store")


## ----Date features
preds <- c()
trts$Year <- year(trts$Date)
trts$Month <- month(trts$Date)
trts$Week <- week(trts$Date)
trts$DayOfMonth <- mday(trts$Date)
trts$DayOfWeek <- wday(trts$Date)
preds <- c(preds, "Year", "Month", "Week", "DayOfMonth", "DayOfWeek")
## ----Replace factors by numerics, and use independent categories
print("Replace factors by numerics")
trts$SchoolHoliday <- as.numeric(trts$SchoolHoliday)
preds <- c(preds, "SchoolHoliday")
print("Though StateHoliday has more than two categories")
print(", they are extremely unbalnced.")
print("Most frequent being not a holiday")
print("We will use a grouped-category representatio for these")
print("EXPERIMENT: use independenent categories for StateHoliday")
trts$StateHoliday <- as.numeric(trts$StateHoliday)
preds <- c(preds, "StateHoliday")
print("Use independent features for store type and assortment.")
print("EXPERIMENT: with grouped categries for these variables.")
storeType.ic <-  independentCategories("StoreType", trts)
trts <- cbind(trts, storeType.ic)
preds <- c(preds, names(storeType.ic))
assortment.ic <- independentCategories("Assortment", trts)
trts <- cbind(trts, assortment.ic)
preds <- c(preds, names(assortment.ic))


## ----Competition

compdate <- with(trts, ymd(paste(CompetitionOpenSinceYear,
                                 CompetitionOpenSinceMonth,
                                 1, sep="-")))
trts$CompetitionOpenDate <- compdate
trts$IsCompetition <- trts$Date > trts$CompetitionOpenDate
trts$CompetitionDuration <- with(trts, (Date - CompetitionOpenDate)/edays(1) )
preds <- c(preds, "IsCompetition", "CompetitionDuration")


## Promotions
promoInterval <- as.integer(factor(store$PromoInterval,
                                   c("", "Jan,Apr,Jul,Oct",
                                     "Feb,May,Aug,Nov",
                                     "Mar,Jun,Sep,Dec"))
                            )
promo2RestartInterval <- promoInterval[trts$Store]
promo2IsBeforeYear <- with(trts, Year > Promo2SinceYear)
promo2IsSameYear <- with(trts, Year == Promo2SinceYear)
promo2IsBeforeWeek <- with(trts, Week > Promo2SinceWeek)
promo2IsValid <- ((promo2IsBeforeYear |
                   (promo2IsSameYear & promo2IsBeforeWeek)) &
                  with(trts, Promo2 == 1))
promo2MonthsSinceRestart <- (trts$Month - promo2RestartInterval + 3) %% 3
promo2MonthsSinceRestart[!promo2IsValid] <- 100 #a large number to indicate something long invalid time ago
promo2MonthsSinceRestart[is.na(promo2MonthsSinceRestart)] <- 100
trts$Promo2MonthsSinceRestart <- promo2MonthsSinceRestart

preds <- c(preds, "Promo2MonthsSinceRestart")

trts$Promo2Duration <- with(trts, 52 * (Year - Promo2SinceYear) +
                                  (Week - Promo2SinceWeek) )
preds <- c(preds, "Promo2Duration")

outcomes <- c("Sales", "LogSales", "DeviationLogSales", "DeviationLogSalesByStore")

trts$LogSales <- log(trts$Sales)
trts$DeviationLogSales <- with(trts, LogSales - mean(LogSales, na.rm=TRUE))
meanLogSalesByStore <- ddply(trts[, c("Store", "LogSales")] , .(Store), summarise,
                             MeanLogSales = mean(LogSales, na.rm=TRUE),
                             SdLogSales = sd(LogSales, na.rm=TRUE))
trts <- merge(trts, meanLogSalesByStore, by = "Store")
trts$DeviationLogSalesByStore <- with(trts, LogSales - MeanLogSales)

preds <- c(preds, 'MeanLogSales', 'SdLogSales')

## some preds that will be used as avaiable in the data,
preds <- union(preds,
               c("Store", "Promo", "CompetitionDistance", "CompetitionOpenSinceMonth",
                 "Open", "CompetitionOpenSinceYear", "Promo2", "PromoInterval",
                 "Promo2SinceWeek", "Promo2SinceYear"))
## some sensible imputation
trts$CompetitionDistance[is.na(trts$CompetitionDistance)] <- 1000000 #to represent an invalid value
trts$CompetitionDuration[is.na(trts$CompetitionDuration)] <- -1000000 #no compettion == competition far in future

##For linear regression we will want factors for DayOfWeek, Month etc. We will use independenent categories

dowcats <- independentCategories(p="DayOfWeek", data=trts)
moncats <- independentCategories(p="Month", data=trts)
yearcats <- independentCategories(p="Year", data=trts)
trts <- cbind(trts, dowcats)
trts <- cbind(trts, moncats)
trts <- cbind(trts, yearcats)

preds <- c(preds, names(dowcats))
preds <- c(preds, names(moncats))
preds <- c(preds, names(yearcats))

## impute NAs in predictor data
## trts.p <- trts[, c("Id", "tr.ts", preds)]
not.preds.outcomes <- setdiff(names(trts), c(preds, outcomes))
trts.not.preds.outcomes <- trts[, not.preds.outcomes]
trts.p <- trts[, preds]
trts.o <- trts[, outcomes]
trts.p[is.na(trts.p)] <- -1
## recombine the predictor and outcome data
trts <- cbind(cbind(trts.p, trts.o), trts.not.preds.outcomes)


## people get paid and spend that money early in the month, or many renew their prescriptions
## early in the month. Lets see how to extract the first week of the month
firstWeekOfMonth <- trts$DayOfMonth < 7
trts$FirstWeekOfMonth <- firstWeekOfMonth
## separate the training and testing data
trp <- subset(trts, tr.ts=="tr")
tsp <- subset(trts, tr.ts=="ts")

## The data is now ready for modelling

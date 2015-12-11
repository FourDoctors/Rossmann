## We have seen that error on held out data from 2 consecutive months is
## higher than randomly held-out data. A tree is constructed by splitting individually,
## rows, which assumes that rows are independent pieces of data. This may not be true
## of the sales data that we have here. 
## There may be correlations among Sales over short periods oftime.
## To catch these correlations, we will predict weekly aggregated sales


preds.weekly <- c("Year", "Month", "DayOfMonth", "DayOfWeek",
                  "SchoolHoliday", "StateHoliday", names(storeType.ic),
                  names(assortment.ic), "IsCompetition", "CompetitionDuration",
                  "CompetitionDistance", "Promo2MonthsSinceRestart", "Promo2Duration",
                  "MeanLogSales", "SdLogSales", "Promo", "PromoInterval")
                  
trts.weekly.tmp <- trts.weekly[, c("Store", "Year", "Week", "DayOfWeek", "LogSales", "tr.ts")]
trts.weekly.tmp$tr.ts <- as.numeric(trts.weekly$tr.ts == "tr")
trts.weekly <- ddply(trts.weekly.tmp,
                     .(Store, Year, Week),
                     summarise,
                     tr.ts = mean(tr.ts, na.rm=TRUE),
                     LogSales = sum(LogSales, na.rm=TRUE),
                     DayOfWeek = mean(DayOfWeek, na.rm=TRUE))


trts.weekly$tr.ts <- sapply(trts.weekly$tr.ts, function(x) {
  if (x < 1.0) "ts" else "tr"
} )


trts.weekly <- merge(trts.weekly, store, by="Store")


## ----Date features
preds.weekly <- c(preds, "Year", "Month", "Week", "DayOfMonth", "DayOfWeek")
## ----Replace factors by numerics, and use independent categories
print("Replace factors by numerics")
trts.weekly$SchoolHoliday <- as.numeric(trts.weekly$SchoolHoliday)
preds <- c(preds, "SchoolHoliday")
print("Though StateHoliday has more than two categories")
print(", they are extremely unbalnced.")
print("Most frequent being not a holiday")
print("We will use a grouped-category representatio for these")
print("EXPERIMENT: use independenent categories for StateHoliday")
trts.weekly$StateHoliday <- as.numeric(trts.weekly$StateHoliday)
preds <- c(preds, "StateHoliday")
print("Use independent features for store type and assortment.")
print("EXPERIMENT: with grouped categries for these variables.")
storeType.ic <-  independentCategories("StoreType", trts.weekly)
trts.weekly <- cbind(trts.weekly, storeType.ic)
preds <- c(preds, names(storeType.ic))
assortment.ic <- independentCategories("Assortment", trts.weekly)
trts.weekly <- cbind(trts.weekly, assortment.ic)
preds <- c(preds, names(assortment.ic))


## ----Competition

compdate <- with(trts.weekly, ymd(paste(CompetitionOpenSinceYear,
                                 CompetitionOpenSinceMonth,
                                 1, sep="-")))
trts.weekly$CompetitionOpenDate <- compdate
trts.weekly$IsCompetition <- trts.weekly$Date > trts.weekly$CompetitionOpenDate
trts.weekly$CompetitionDuration <- with(trts.weekly, (Date - CompetitionOpenDate)/edays(1) )
preds <- c(preds, "IsCompetition", "CompetitionDuration")


## Promotions
promoInterval <- as.integer(factor(store$PromoInterval,
                                   c("", "Jan,Apr,Jul,Oct",
                                     "Feb,May,Aug,Nov",
                                     "Mar,Jun,Sep,Dec"))
                            )
promo2RestartInterval <- promoInterval[trts.weekly$Store]
promo2IsBeforeYear <- with(trts.weekly, Year > Promo2SinceYear)
promo2IsSameYear <- with(trts.weekly, Year == Promo2SinceYear)
promo2IsBeforeWeek <- with(trts.weekly, Week > Promo2SinceWeek)
promo2IsValid <- ((promo2IsBeforeYear |
                   (promo2IsSameYear & promo2IsBeforeWeek)) &
                  with(trts.weekly, Promo2 == 1))
promo2MonthsSinceRestart <- (trts.weekly$Month - promo2RestartInterval + 3) %% 3
promo2MonthsSinceRestart[!promo2IsValid] <- Inf
trts.weekly$Promo2MonthsSinceRestart <- promo2MonthsSinceRestart

preds <- c(preds, "Promo2MonthsSinceRestart")

trts.weekly$Promo2Duration <- with(trts.weekly, 52 * (Year - Promo2SinceYear) +
                                  (Week - Promo2SinceWeek) )
preds <- c(preds, "Promo2Duration")

outcomes <- c("Sales", "LogSales", "DeviationLogSales", "DeviationLogSalesByStore")

trts.weekly$LogSales <- log(trts.weekly$Sales)
trts.weekly$DeviationLogSales <- with(trts.weekly, LogSales - mean(LogSales, na.rm=TRUE))
meanLogSalesByStore <- ddply(trts.weekly[, c("Store", "LogSales")] , .(Store), summarise,
                             MeanLogSales = mean(LogSales, na.rm=TRUE),
                             SdLogSales = sd(LogSales, na.rm=TRUE))
trts.weekly <- merge(trts.weekly, meanLogSalesByStore, by = "Store")
trts.weekly$DeviationLogSalesByStore <- with(trts.weekly, LogSales - MeanLogSales)

preds <- c(preds, 'MeanLogSales', 'SdLogSales')

## some preds that will be used as avaiable in the data,
preds <- union(preds,
               c("Store", "Promo", "CompetitionDistance", "CompetitionOpenSinceMonth",
                 "Open", "CompetitionOpenSinceYear", "Promo2", "PromoInterval",
                 "Promo2SinceWeek", "Promo2SinceYear"))
## some sensible imputation
trts.weekly$CompetitionDistance[is.na(trts.weekly$CompetitionDistance)] <- Inf
trts.weekly$CompetitionDuation[is.na(trts.weekly$CompetitionDuration)] <- -Inf
## impute NAs in predictor data
## trts.weekly.p <- trts.weekly[, c("Id", "tr.ts", preds)]
##For linear regression we will want factors for DayOfWeek, Month etc. We will use independenent categories

dowcats <- independentCategories(p="DayOfWeek", data=trts.weekly)
moncats <- independentCategories(p="Month", data=trts.weekly)
yearcats <- independentCategories(p="Year", data=trts.weekly)
trts.weekly <- cbind(trts.weekly, dowcats)
trts.weekly <- cbind(trts.weekly, moncats)
trts.weekly <- cbind(trts.weekly, yearcats)

preds <- c(preds, names(dowcats))
preds <- c(preds, names(moncats))
preds <- c(preds, names(yearcats))

## sales change over weeks, quite significantly
weeklyMeansScaledByStore <- ddply(trts.weekly, .(Store, Week), summarise,
                                 ScaledWeeklyMean = (mean(LogSales, na.rm=TRUE) -
                                         storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])
trts.weekly <- merge(trts.weekly, weeklyMeansScaledByStore, by = c("Store", "Week"))
preds <- c(preds, "ScaledWeeklyMean")
monthlyMeansScaledByStore <- ddply(trts.weekly, .(Store, Month), summarise,
                                 ScaledMonthlyMean = (mean(LogSales, na.rm=TRUE) -
                                         storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])
trts.weekly <- merge(trts.weekly, monthlyMeansScaledByStore, by = c("Store", "Month"))
preds <- c(preds, "ScaledMonthlyMean")

##going on with scaled means, lets also have a scaled mean for each store

scaledMeanLogSales <- ddply(trts.weekly, .(Store), summarise,
                            ScaledStoreMean = (mean(LogSales, na.rm=TRUE) -
                                               mean(trts.weekly$LogSales, na.rm=TRUE)) / sd(trts.weekly$LogSales, na.rm=TRUE)
                            )
trts.weekly <- merge(trts.weekly, scaledMeanLogSales, by="Store")
preds <- c(preds, "ScaledStoreMean")


print("cluster the stores by monthly sales")
monthMeansByStoreWide <- dcast(monthMeansScaledByStore, formula = Store ~  Month, value.var = "mean")
monthlySalesDists <- as.matrix(dist(monthMeansByStoreWide[, -1], method="euclidean"))
clu <- hclust(dist(monthMeansByStoreWide[, -1], method="euclidean"), method="ward.D")
clucut <- cutree(clu, k = 4)
mmsw <- cbind(monthMeansByStoreWide, clucut)
trts.weekly <- merge(trts.weekly, mmsw[, c("Store", "clucut")], by="Store")
preds <- c(preds, "clucut") 
##a look at plots of mean log sales against day of month suggests that there are three
##distinct types of days in a month: sales decrease during the first 11 days, then increases
##and decreases during the next 15, and finally increases to a monthly maximum at
##the end of the month. We thus introduce a new variable,
periodInMonth <- with(trts.weekly, 1*(DayOfMonth < 11) +
                            2*(DayOfMonth >= 11 & DayOfMonth < 25) +
                            3 *(DayOfMonth >= 25))
trts.weekly$PeriodInMonth <- periodInMonth
preds <- c(preds, "PeriodInMonth")


print("cluster the months")
monthMeansWideByMonth <- dcast(monthMeansScaledByStore,
                               formula = Month ~ Store,
                               value.var = "mean")
distMonths <- as.matrix(dist(monthMeansWideByMonth[, -1]), method="euclidean")
cluMonths <- hclust(dist(monthMeansWideByMonth, method="euclidean"),
                    method="ward.D")
clucutMonths <- cutree(cluMonths, k = 4)
monthMeansWideByMonth <- cbind(monthMeansWideByMonth, clucutMonths)
trts.weekly <- merge(trts.weekly, monthMeansWideByMonth[, c("Month", "clucutMonths")], by="Month")
preds <- c(preds, "clucutMonths")

print("cluster the weeks")
weeklyMeansScaledByStore <- ddply(trts.weekly, .(Store, Week), summarise,
                                 mean = (mean(LogSales, na.rm=TRUE) -
                                         storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])

weekMeansWideByWeek <- dcast(weeklyMeansScaledByStore,
                             formula = Week ~ Store,
                             value.var="mean" )
distWeeks <- as.matrix(dist(weekMeansWideByWeek[, -1], method="euclidean"))
cluWeeks <- hclust(dist(weekMeansWideByWeek[, -1], method="euclidean"),
                   method="ward.D")
clucutWeeks <- cutree(cluWeeks, k=5)
weekMeansWideByWeek <- cbind(weekMeansWideByWeek, clucutWeeks)
trts.weekly <- merge(trts.weekly, weekMeansWideByWeek[, c("Week", "clucutWeeks")], by="Week")
preds <- c(preds, "clucutWeeks")

##use customers to cluster the weeks
weeklyCustByStore <- ddply(trp, .(Store, Week), summarise,
                           mean = mean(log(Customers), na.rm=TRUE))
weeklyCustWideByWeek <- dcast(weeklyCustByStore,
                              formula = Week ~ Store,
                              value.var = "mean")
distWeeksByCust <- as.matrix(dist(weeklyCustWideByWeek[, -1], method="euclidean"))
cluWeeksByCust <- hclust(dist(weeklyCustWideByWeek[, -1], method="euclidean"),
                         method = "ward.D")
clucutWeekByCust <- cutree(cluWeeksByCust, k = 3)
weeklyCustWideByWeek <- cbind(weeklyCustWideByWeek, clucutWeekByCust)
trts.weekly <- merge(trts.weekly, weeklyCustWideByWeek[, c("Week", "clucutWeekByCust")], by = "Week")
preds <- c(preds, "clucutWeekByCust")
## impute NAs in predictor data
## trts.weekly.p <- trts.weekly[, c("Id", "tr.ts", preds)]
not.preds.outcomes <- setdiff(names(trts.weekly), c(preds, outcomes))
trts.weekly.not.preds.outcomes <- trts.weekly[, not.preds.outcomes]
trts.weekly.p <- trts.weekly[, preds]
trts.weekly.o <- trts.weekly[, outcomes]
trts.weekly.p[is.na(trts.weekly.p)] <- -1
## recombine the predictor and outcome data
trts.weekly <- cbind(cbind(trts.weekly.p, trts.weekly.o), trts.weekly.not.preds.outcomes)

## separate the training and testing data
trp <- subset(trts.weekly, tr.ts=="tr")
tsp <- subset(trts.weekly, tr.ts=="ts")

print("The data is now ready for modelling")

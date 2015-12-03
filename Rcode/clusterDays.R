monthMeansWideByMonth <- dcast(monthMeansScaledByStore,
                               formula = Month ~ Store,
                               value.var = "mean")
distMonths <- as.matrix(dist(monthMeansWideByMonth[, -1]), method="euclidean")
cluMonths <- hclust(dist(monthMeansWideByMonth, method="euclidean"), method="ward.D")
clucutMonths <- cutree(cluMonths, k = 4)
monthMeansWideByMonth <- cbind(monthMeansWideByMonth, clucutMonths)

weekMeansWideByWeek <- dcast(weeklyMeansScaledByStore,
                             formula = Week ~ Store,
                             value.var="mean" )
distWeeks <- as.matrix(dist(weekMeansWideByWeek[, -1], method="euclidean"))
cluWeeks <- hclust(dist(weekMeansWideByWeek[, -1], method="euclidean"),
                   method="ward.D")
clucutWeeks <- cutree(cluWeeks, k=4)
weekMeansWideByWeek <- cbind(weekMeansWideByWeek, clucutWeeks)

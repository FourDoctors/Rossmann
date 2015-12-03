## study monthly sales


storeMeans <- ddply(trp.tr, .(Store), summarise,
                    mean=mean(LogSales, na.rm=TRUE),
                    sd = sd(LogSales, na.rm=TRUE))

monthMeansScaledByStore <- ddply(trts, .(Store, Month), summarise,
                                 mean = (mean(LogSales, na.rm=TRUE) -
                                         storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])

monthMeansScaledByStoreAndYear <- ddply(trts, .(Store, Month, Year), summarise,
                                        mean = (mean(LogSales, na.rm=TRUE) -
                                                storeMeans$mean[storeMeans$Store == unique(Store)]
                                        )/storeMeans$sd[storeMeans$Store == unique(Store)])

monthMeansByStoreAndYear <- ddply(trts, .(Store, Month, Year), summarise,
                                  mean = mean(LogSales, na.rm=TRUE)
                                                
                                        )/storeMeans$sd[storeMeans$Store == unique(Store)])
monthMeansByStoreWide <- dcast(monthMeansScaledByStore, formula = Store ~  Month,
                               value.var = "mean")
monthlySalesDists <- as.matrix(dist(monthMeansByStoreWide[, -1], method="euclidean"))
clu <- hclust(dist(monthMeansByStoreWide[, -1], method="euclidean"), method="ward.D")
clucut <- cutree(clu, k = 4)
mmsw <- cbind(monthMeansByStoreWide, clucut)
              
Mmsw.m <- melt(mmsw[, -1], id.vars=.(clucut), variable.name="month", value.name="mean")
mmsw.m$Store <- 1:nrow(mmsw.m)
xyplot(mean ~ month,
       groups=clucut,
       auto.key=TRUE,
       type="b",
       data = ddply(mmsw.m, .(clucut, month), summarize, mean=mean(mean)))


weeklyMeansScaledByStore <- ddply(trts, .(Store, Week), summarise,
                                 mean = (mean(LogSales, na.rm=TRUE) -
                                         storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])

weeklyMeansScaledByStoreAndYear <- ddply(trts, .(Store, Week, Year), summarise,
                                         mean = (mean(LogSales, na.rm=TRUE) -
                                                 storeMeans$mean[storeMeans$Store == unique(Store)]
                                 )/storeMeans$sd[storeMeans$Store == unique(Store)])
weeklyMeansScaled <- ddply(weeklyMeansScaledByStore, .(Week), summarise,
                           mean = mean(mean, na.rm=TRUE),
                           sd = sd(mean, na.rm=TRUE))

weeklyMeans <- ddply(trts, .(Week), summarise,
                     mean = (mean(LogSales, na.rm=TRUE) -
                             mean(trts$LogSales, na.rm=TRUE)
                     )/sd(trts$LogSales, na.rm=TRUE))


weeklyCustomersByStoreAndYear <- ddply(trp, .(Store, Week, Year), summarise,
                                       mean = mean(log10(Customers), na.rm=TRUE))

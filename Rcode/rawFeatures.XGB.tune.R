source("tuningXGB.r")
outcome <- "DeviationLogSales"
tunexgb(training = trp[, preds],
        label = trp[, outcome],
        grid = expand.grid(max.depth = c(8, 10, 12),
                           eta = c(0.1, 0.25, 0.3, 0.5)),
        nrounds = 300,
        subsample = 0.9,
        colsample_bytree = 0.5,
        num.threads = 4,
        flabel = paste("non_time-series.outcome", outcome, sep="_") )


        

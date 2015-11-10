tunexgb <- function(
    training,
    label,
    grid = expand.grid(max.depth = c(6, 9),
                       eta = c(0.01, 0.1, 1.0)),
    subsample = 0.7,
    colsample_bytree = 0.7,
    silent = 1,
    seed = 1301,
    nrounds = 100,
    num.threads = 3,
    flabel = "") {

    for( n in 1:nrow(grid)) {
        md <- grid$max.depth[n]
        eta <- grid$eta[n]
        print(paste("max.depth", md, "eta", eta))
        df.cv <- xgb.cv(param = list(max.depth = md,
                                     eta = eta,
                                     silent = silent,
                                     nthread = num.threads,
                                     objective = "reg:linear",
                                     seed = seed,
                                     subsample = subsample,
                                     colsample_bytree = colsample_bytree),
                        data = xgb.DMatrix(data.matrix(training),
                                           label = label),
                        nrounds = nrounds,
                        early_stopping_rounds=100,
                        nfold = 5,
                        metrics = {'rmse'} )
        df.cv$nrounds <- 1:nrounds
        df.cv$max.depth <- md
        df.cv$eta <- eta
        write.csv(df.cv, file=paste("../data/tunings/xgbTuning",
                                    "max_depth", md, "eta", eta,
                                    flabel,
                                    "csv", sep=".")
                  )
    }
}

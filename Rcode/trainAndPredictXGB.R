train.xgb <- function(datasets,
                      preds,
                      outcome,
                      nrounds = 100,
                      max.depth = 12,
                      eta = 0.3,
                      subsample = 0.7,
                      colsample_bytree = 0.7,
                      num.threads = 4,
                      early.stop.round = NULL) {
  training <- datasets$training
  dtrain <- xgb.DMatrix(data = data.matrix(training[, preds]),
                        label = data.matrix(training[, outcome]))
  dtest <- lapply(datasets$testing, function(testing) {
    xgb.DMatrix(data = data.matrix(testing[, preds]),
                label = data.matrix(testing[, outcome]))
  })
                                        #watchlist <- list(train = dtrain, test = dtest)
  watchlist <- c( dtest, train = dtrain)

  clf <- xgb.train(data = dtrain,
                   max.depth = max.depth,
                   eta = eta,
                   nthread = num.threads,
                   nround = nrounds,
                   watchlist = watchlist,
                   objective = "reg:linear",
                   subsample = subsample,
                   colsample_bytree = colsample_bytree,
                                        #silent = 1,
                   eval_metric = "rmse",
                   early.stop.round = early.stop.round)
  
  prediction.train <- predict(clf, dtrain)
  prediction.test <- predict(clf, dtest)
  
  print("training error")
  x <- exp(training[, outcome])
  y <- exp(prediction.train)
  train.err <- sqrt(mean((y/x - 1)^2))
  print(train.err)
  
  print("testing error")
  x <- exp(testing[, outcome])
  y <- exp(prediction.test)
  test.err <- sqrt(mean((y/x - 1)^2))
  print(test.err)
  
  clf
}



predictAndSave <- function() {
  subm <- data.frame(Id = testing[, testid], Sales = exp(prediction.test))
  paramstring <- paste(
    paste('nrounds', nrounds, sep="_"),
    paste('maxDepth', max.depth, sep="_"),
    paste('eta', eta, sep="_"),
    sep = ".")
  filename <- paste("../data/predictions/",
                    paste("prediction_xgb", paramstring, sep="."),
                    sep="")
  filename <- paste(filename, fnlabel, "csv", sep=".")
  write.csv(subm, file = filename, row.names=FALSE)
  filename
}

                                        #fn <- trainAndPredict.xgb(training = training.trn,
                                        #                          label = log(training.trn$Sales),
                                        #                          testing = training.holdout,
                                        #                          testid = "Id",
                                        #                          nrounds = 150,
                                        #                          max.depth = 10,
                                        #                          eta = 0.3,
                                        #                          subsample=0.7,
                                        #                          colsample_bytree=0.7,
                                        #                          silent=1,
                                        #                          num.threads=4)



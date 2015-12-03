train.xgb <- function(
                      training,
                      testing,
                      preds,
                      outcome,
                      booster = "gblinear",
                      nrounds = 100,
                      num.threads = 4,
                      early.stop.round = NULL,
                      lambda = 0,
                      alpha = 0,
                      lambda_bias = 0
                      ) {

  dtrain <- xgb.DMatrix(data = data.matrix(training[, preds]),
                        label = data.matrix(training[, outcome]))
  dtest <- xgb.DMatrix(data = data.matrix(testing[, preds]),
                       label = data.matrix(testing[, outcome]))
  watchlist <- list( test = dtest, train = dtrain)
  
  clf <- xgb.train(data = dtrain,
                   booster = "gblinear",
                   nthread = num.threads,
                   nround = nrounds,
                   watchlist = watchlist,
                   objective = "reg:linear",
                   eval_metric = "rmse",
                   early.stop.round = early.stop.round,
                   lambda = lambda,
                   alpha = alpha,
                   lambda_bias = lambda_bias)

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



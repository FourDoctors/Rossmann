## Script to train
## Please see the associated .produce_data.R file for how to generate data used here.

## ----heldout----------------

print("For simple validation, hold out a small data-set for internal testing")
set.seed(210)
idxho <- sample(1:nrow(trp), 0.0125*nrow(trp))
trp.ho <- trp[idxho, ]
trp.tr <- trp[-idxho, ]

##------train---------

source("../Rcode/trainAndPredictXGB.R")
params <- c(nrounds = 200,
            maxDepth = 10,
            subsample = 0.9,
            colsample_bytree = 0.9,
            eta = 0.3)
outcome <- "LogSales"

print("run for params ")
print(params)
print(" and outcome")
print(outcome)
bst <- train.xgb(training = trp.tr,
                 testing = trp.ho,
                 preds = preds,
                 outcome = outcome,
                 nrounds = params["nrounds"],
                 max.depth = params["maxDepth"],
                 eta = params["eta"],
                 subsample = params["subsample"],
                 colsample_bytree = params["colsample_bytree"],
                 num.threads = 4
                 )
dn <- "../data/predictions/prediction_xgb_non_time-series_models"
fndf <- data.frame(
    name = c(names(params), "outcome"),
    value = as.character(c(params, outcome)),
    stringsAsFactors = FALSE
)
fn <- paste(dn,
            paste(paste(fndf$name, fndf$value, sep="_"),
                  collapse="."),
            "csv",
            sep=".")
if (outcome == "DeviationLogSales") {
    prho <- exp( mean(trp.ho$LogSales) +
                predict(bst, data.matrix(trp.ho[, preds])))
    prtst <- data.frame(Id = tsp$Id,
                        Sales = exp(mean(tsp$LogSales) +
                                    predict(bst, data.matrix(tsp[, preds])))
                        )
}

if (outcome == "DeviationLogSalesByStore") {
  prho <- exp( trp.ho$MeanLogSales +
               predict(bst, data.matrix(trp.ho[, preds])))
  prtst <- data.frame(Id = tsp$Id,
                      Sales = exp( tsp$MeanLogSales +
                                   predict(bst, data.matrix(tsp[, preds])))
                      )
}

if(outcome == "LogSales") {
  prho <- exp( predict(bst, data.matrix(trp.ho[, preds])))
  prtst <- data.frame(Id = tsp$Id,
                      Sales = exp( predict(bst, data.matrix(tsp[, preds])))
                      )
}

  

print("held out rms error")
trp.ho$Sales <- exp(trp.ho$LogSales)
print( sqrt(mean((prho/trp.ho$Sales - 1)^2)))
prtst <- prtst[order(prtst$Id),]
write.csv(prtst, file = fn, row.names=FALSE)










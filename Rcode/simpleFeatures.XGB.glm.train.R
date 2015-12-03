## since the variable names are the same,
## we can copy the script used for raw feature training

require(xgboost)
source("../Rcode/trainAndPredictXGB.glm.R")
print("For simple validation, hold out a small data-set for internal testing")
set.seed(210)
idxho <- sample(1:nrow(trp), 0.0125 * nrow(trp))
trp.ho <- trp[idxho, ]
trp.tr <- trp[-idxho, ]

load.data = FALSE
if (load.data == TRUE)
    source("../Rcode/simpleFeatures.glm.R")
params <- c(nrounds = 10000,
            lambda = 10.0,
            alpha = 5.0,
            lambda_bias = 10.0)
            

print("run for params ")
print(params)
print(" and outcome")
print(outcome)
bst <- train.xgb(training = trp.tr,
                 testing = trp.ho,
                 preds = preds,
                 outcome = outcome,
                 booster = "gblinear",
                 nrounds = params["nrounds"],
                 num.threads = 6,
                 early.stop.round=10,
                 lambda = params['lambda'],
                 alpha = params['alpha'],
                 lambda_bias = params['lambda_bias']
                 )
dn <- "../data/predictions/simpleFeatures/gblinear/prediction_xgb_non_time-series_models"
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









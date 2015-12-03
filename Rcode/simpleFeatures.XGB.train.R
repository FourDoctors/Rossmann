## since the variable names are the same,
## we can copy the script used for raw feature training

source("../Rcode/trainAndPredictXGB.R")
print("For simple validation, hold out a small data-set for internal testing")
set.seed(701)
if (take2RandomMonths) {
  y <- sample(unique(trp$Year), 1)
  m <- sample(unique(trp$Month[trp$Year == y]), 2)
  print(paste("take month ", m, "from year", y))
  idxho.m <- which(trp$Month %in% m & trp$Year == y)
} else {
  idxho.m <- c()
}
idxho.r <- sample(setdiff(1:nrow(trp), idxho.m),
                  randomFraction * nrow(trp))
trp.mho <- trp[idxho.m,]
trp.rho <- trp[idxho.r,]
idxho <- union(idxho.r, idxho.m)
trp.ho <- trp[idxho, ]
trp.tr <- trp[-idxho, ]

load.data = FALSE
if (load.data == TRUE)
    source("../Rcode/simpleFeatures.R")
params <- c(nrounds = 200,
            maxDepth = 10,
            subsample = 0.50,
            colsample_bytree = 0.90,
            eta = 0.3)

print("run for params ")
print(params)
print(" and outcome")
print(outcome)
bst <- train.xgb(datasets = list(training = trp.tr,
                                 testing = list(m = trp.mho, r = trp.rho)),
                 preds = preds,
                 outcome = outcome,
                 nrounds = params["nrounds"],
                 max.depth = params["maxDepth"],
                 eta = params["eta"],
                 subsample = params["subsample"],
                 colsample_bytree = params["colsample_bytree"],
                 num.threads = 6,
                 early.stop.round=10
                 )
dn <- "../data/predictions/simpleFeatures/prediction_xgb_non_time-series_models"
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
  prho.r <- exp( predict(bst, data.matrix(trp.rho[, preds])))
  prho.m <- exp( predict(bst, data.matrix(trp.mho[, preds])))
  prtst <- data.frame(Id = tsp$Id,
                      Sales = exp( predict(bst, data.matrix(tsp[, preds])))
                      )
}

  

print("held out rms error")
print("all held out")
trp.ho$Sales <- exp(trp.ho$LogSales)
print( sqrt(mean((prho/trp.ho$Sales - 1)^2)))
print("random held out")
trp.rho$Sales <- exp(trp.rho$LogSales)
print( sqrt(mean((prho.r/trp.rho$Sales - 1)^2)))
print("2 months held out")
trp.mho$Sales <- exp(trp.mho$LogSales)
print( sqrt(mean((prho.m/trp.mho$Sales - 1)^2)))
prtst <- prtst[order(prtst$Id),]
write.csv(prtst, file = fn, row.names=FALSE)









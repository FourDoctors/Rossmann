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

bst <- train.xgb(training = trp.tr,
                 testing = trp.ho,
                 preds = preds,
                 outcome = "DeviationLogSales",
                 nrounds = 300,
                 max.depth = 10,
                 eta = 0.3,
                 subsample = 0.7,
                 colsample_bytree = 0.7,
                 num.threads = 4
                 )

prho <- exp( mean(trp.tr$LogSales) +
             predict(bst, data.matrix(trp.ho[, preds])))
print("held out rms error")
trp.ho$Sales <- exp(trp.ho$LogSales)
print( mean(sqrt((prho/trp.ho$Sales - 1)^2)))

prtst <- data.frame(Id = tsp$Id,
                    Sales = exp(mean(trp.tr$LogSales) +
                                predict(bst, data.matrix(tsp[, preds]))
                                )
                    )
prtst <- prtst[order(prtst$Id),]

dn <- "../data/predictions/prediction_xgb_non_time-series_models"
params <- c(nrounds = 300,
            maxDepth = 10,
            eta = 0.3)
outcome = "DeviationLogSales"
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
write.csv(prtst, file = fn, row.names=FALSE)













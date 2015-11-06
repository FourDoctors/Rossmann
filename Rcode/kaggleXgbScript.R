
## ----loaddata-------------

training <- read.csv("../data/train.csv")
testing <- read.csv("../data/test.csv")
store <- read.csv("../data/store.csv")

training$Date <- ymd(training$Date)
testing$Date <- ymd(testing$Date)


## ----useonlyopenstores---------------------------------------------------

testing$Open[is.na(testing$Open)] <- 1
training <- subset(training, Open != 0)
training <- subset(training, Sales > 0)

##----add remove cols------
testing$Sales <- NA
testing$Customers <- NA
training$Id <- 1:nrow(training)

##----combine test train
training$tr.ts <- "tr"
testing$tr.ts <- "ts"
trts <- rbind(training, testing)


## ----merge store---------------------------------------------------------

trts <- merge(trts, store, by="Store")

## ----featuredirect-------------------------------------------------------

preds <- c('Store', 'CompetitionDistance',
           'CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear',
           'Promo', 'Promo2',
           'Promo2SinceWeek', 'Promo2SinceYear')


## ----processedFeatures---------------------------------------------------

trts$SchoolHoliday <- as.numeric(trts$SchoolHoliday)
preds <- c(preds, "SchoolHoliday")
trts$StateHoliday <- as.numeric(trts$StateHoliday)
preds <- c(preds, "StateHoliday")
trts <- cbind(trts,
              data.frame(
                  Year = year(trts$Date),
                  Month = month(trts$Date),
                  DayOfWeek = wday(trts$Date),
                  DayOfMonth = mday(trts$Date))
              )
preds <- c(preds, "Year", "Month", "DayOfWeek", "DayOfMonth")

storeType.ic <- independentCategories( "StoreType", trts)
trts <- cbind(trts, storeType.ic)
preds <- c(preds, names(storeType.ic))
assortmnent.ic <- independentCategories("Assortment", trts)
trts <- cbind(trts, assortmnent.ic)
preds <- c(preds, names(assortmnent.ic))

##---- extract training and testing out of trts
trts$LogSales <- log(trts$Sales)
trts.p <- trts[, c("Id", "tr.ts", preds)]
trts.o <- trts$LogSales

trts.p[is.na(trts.p)] <- 0
trts <- trts.p
trts$LogSales <- trts.o


trp <- subset(trts, tr.ts=="tr")
trp$DeviationLogSales <- with(trp, LogSales - mean(LogSales))
tsp <- subset(trts, tr.ts=="ts")

## ----heldout----------------

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

dn <- "../data/predictions/prediction_xgb"
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













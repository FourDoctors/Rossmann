plotXGBtuning <- function(df.tuning, logscale = TRUE) {

    df.tuning.ml <- melt(df.tuning[, c("nrounds", "max.depth", "eta",
                                       "train.rmse.mean", "test.rmse.mean")],
                         id.vars = c("nrounds", "max.depth", "eta"))
    if (logscale) df.tuning.ml$value = log10(df.tuning.ml$value)
    xyplot(value ~ nrounds | variable + as.factor(eta),
           groups = as.factor(max.depth),
           data=df.tuning.ml,
           type = "b",
           auto.key = TRUE,
           main = "Tuning curves for XGBOOST ")


}

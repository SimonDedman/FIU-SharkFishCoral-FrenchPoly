# 2024-07-08 Bayesian Additive Regression Trees BART test on DAG-consistency-checked data
# Run TeleostFunctionalGroupDiets.qmd then nat-FPDAG_consistency_check.R first.
install.packages("BART")
library(BART)

# Sharks influence coral ####
dat <- read.csv("/home/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/ReefWideBRUVUVC-DAGtested.csv") # read data. Don't use read_csv: creates tibble which breaks gbart
dat <- read.csv("C:/Users/simon/Documents/Si Work/PostDoc Work/FIU/2024-01_SharksFishCoral-FrenchPoly/NFF Data code/ReefWideBRUVUVC-DAGtested.csv") # read data. Don't use read_csv: creates tibble which breaks gbart
train <- sample(1:nrow(dat), nrow(dat) / 2) # 50% test/train proportion
expvars <- c("reef_sharks", "piscivores", "pop_dens", "sicklefin_lemon_sharks", "transient_pelagic_sharks")
x <- dat[, expvars] # set explanatory variables ("exposures" + minimal sufficient adjustment sets)
y <- dat[, "hard_coral"] # set response variable ("outcome")
xtrain <- x[train, ] # create training subset of explanatory variables
ytrain <- y[train] # create training subset of response variable
xtest <- x[-train, ] # create testing subset of explanatory variables
ytest <- y[-train] # create testing subset of response variable
bartfit <- gbart(x.train = xtrain,
                 y.train = ytrain,
                 x.test = xtest) # fit BART to training data
yhat.bart <- bartfit$yhat.test.mean # computer test error, Mean Squared Error, MSE
mean((ytest - yhat.bart)^2)
# 0.06191335
sqrt(mean((ytest - yhat.bart)^2)) # square root of MSE is root mean squared error, RMSE
# 0.2488239

# how many times each variable appeared in the collection of trees:
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]
# sicklefin_lemon_sharks               piscivores              reef_sharks                 pop_dens transient_pelagic_sharks
#                 40.791                   38.283                   38.224                   37.673                    0.000

# other metrics / diagnoses / plots?
bartfit$yhat.train # matrix of predictions. Columns = different covariate settings. Rows = the M draws from the posterior.
bartfit$yhat.test # matrix of predictions for x.test
bartfit$yhat.train.mean # The posterior estimate of f(xi), i.e., M−1 SUMm fm (xi)




# Coral influences sharks ####
train <- sample(1:nrow(dat), nrow(dat) / 2) # 50% test/train proportion
expvars <- c("hard_coral", "ave_npp", "herbivores", "invertivores", "planktivores", "pop_dens", "relief") # exposure/outcome switched but arrows down
expvars <- c("ave_npp", "ave_temp", "crustose_coraline_algae", "invertivores", "island_geomorphology", "other_algae", "planktivores", "pop_dens", "relief")
# had to remove s from invertivores & planktivores
x <- dat[, expvars] # set explanatory variables ("exposures" + minimal sufficient adjustment sets)
y <- dat[, "reef_sharks"] # set response variable ("outcome")
xtrain <- x[train, ] # create training subset of explanatory variables
ytrain <- y[train] # create training subset of response variable
xtest <- x[-train, ] # create testing subset of explanatory variables
ytest <- y[-train] # create testing subset of response variable
bartfit <- gbart(x.train = xtrain,
                 y.train = ytrain,
                 x.test = xtest) # fit BART to training data
yhat.bart <- bartfit$yhat.test.mean # computer test error, Mean Squared Error, MSE
mean((ytest - yhat.bart)^2)
# 0.08514577
# 0.01663494 BU arrows up
sqrt(mean((ytest - yhat.bart)^2)) # square root of MSE is root mean squared error, RMSE
# 0.2917975
# 0.1289765 BU arrows up
0.2917975 / 0.2488239 # = 1.172707X worse than sharks influencing coral.

# how many times each variable appeared in the collection of trees:
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]
# herbivores   hard_coral     pop_dens invertivores planktivores      ave_npp       relief
#     26.732       24.874       23.378       22.910       22.796       22.707       19.179

# other metrics / diagnoses / plots?
bartfit$yhat.train # matrix of predictions. Columns = different covariate settings. Rows = the M draws from the posterior.
bartfit$yhat.test # matrix of predictions for x.test
bartfit$yhat.train.mean # The posterior estimate of f(xi), i.e., M−1 SUMm fm (xi)

# PDP, Sparapani et al 2021 p15
for (expvar in expvars) { # expvar <- expvars[1]
H <- length(ytrain)
L <- 41 # why 41?
xrange <- seq(min(xtrain[, expvar]), max(xtrain[, expvar]), length.out = L)
which(expvar == colnames(xtrain))
xtest <- cbind(xtrain[, -which(expvar == colnames(xtrain))], xrange[1])
for (j in 2:L) xtest <- rbind(xtest, cbind(xtrain[, -which(expvar == colnames(xtrain))], xrange[j]))
## FAILS ####
# Error in match.names(clabs, names(xi)): names do not match previous names
pred <- predict(bartfit, xtest)
partial <- matrix(nrow = 1000, ncol = L)
for (j in 1:L) {
    h <- (j - 1) * H + 1:H
    partial[, j] <- apply(pred[, h], 1, mean)
    }
plot(xrange, apply(partial, 2, mean), type = "l", ylim = c(10, 50), xlab = "lstat", ylab = "mdev")
lines(xrange, apply(partial, 2, quantile, probs = 0.025), lty = 2)
lines(xrange, apply(partial, 2, quantile, probs = 0.975), lty = 2)
}


# embarcadero ####
devtools::install_github('cjcarlson/embarcadero')
library(embarcadero)
xnames <- c("x1","x2","x3","x4","x5","x6","x7","x8")
# Run the BART model
sdm <- bart(y.train = occ.df[,"Observed"],
            x.train = occ.df[,xnames],
            keeptrees = TRUE)
# Predict the species distribution
map <- predict(sdm, climate)
# Visualize model performance
summary(bart)
# to derive a 95% credible interval from the 2.5% and 97.5% quantiles of the posterior, a user can specify:
map <- predict(sdm, climate, quantiles = c(0.025, 0.975))
# the function returns a stack of rasters with each specified quantile.
# Mapping the difference be- tween the two quantile rasters gives the credible interval width,
# which provides a native measure of spartial uncertainty,
# analogous to how the coefficient of variation can be used to measure spartial uncertainty across an ensemble of BRT runs

# variable importance:
varimp.diag(occ.df[,xnames],
            occ.df[,"Observed"],
            iter = 50)
step.model <- variable.step(x.data = occ.df[,xnames],
                            y.data = occ.df[,"Observed"])
step.model

# full automated variable selection pipeline = bart.step, which
# (a) produces the initial multi-m diagnostic plot
# (b) runs automated variable selection
# (c) returns a model trained with the optimal variable set
# (d) plots variable importance in the final model
# (e) returns the summary of the final model.


# PDPs:
partial(sdm,
        x.vars = c("x4"),
        smooth = 5,
        equal = TRUE,
        trace = FALSE)

# spartial partial dependence plots (spartial function)
# reclassify predictor rasters based on their partial dependence plots,
# and show the relative suitability of different regions for an individual covariate

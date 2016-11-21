library(mlr)
options(scipen = 999)
data(iris)

## Define the task
task = makeClassifTask(id = "tutorial", data = iris, target = "Species")

lrn = makeLearner("classif.h2o.randomForest")
rdesc = makeResampleDesc(method = "CV", stratify = TRUE)
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)

r$aggr

data(GermanCredit, package = "caret")
abril_dataset_training$participa <- NULL
abril_dataset_training$problemafin <- factor(abril_dataset_training$problemafin)
abril_dataset_training$c_ctactel_visafinanl <- NULL
abril_dataset_training$lim_visavsmaster <- NULL
abril_dataset_training$c_margen_ <- NULL
abril_dataset_training$atrasox90dias <- NULL
abril_dataset_training$sharextvisa <- NULL
abril_dataset_training$activosxmargen <- NULL
abril_train <- cbind.data.frame(abril_dataset_training[,1:160],clase_binaria1=abril_dataset_training$clase_binaria1)
abril_train$participa <- NULL

credit.task = makeClassifTask(data = abril_train, target = "clase_binaria1")
credit.task = removeConstantFeatures(credit.task)
credit.task

costs = matrix(c(7750,-250,-8000, 0 ), 2)
costs
colnames(costs) = rownames(costs) = getTaskClassLevels(credit.task)
costs
lrn = makeLearner("classif.multinom", predict.type = "prob", trace = FALSE)
lrn = makeLearner("classif.ranger",predict.type = "prob",num.threads=4)
mod = train(lrn, credit.task)
pred = predict(mod, task = credit.task)

costs
credit.costs = makeCostMeasure(id = "costs", minimize = FALSE,name = "baja costs", costs = costs)
credit.costs
th = costs[2,1]/(costs[2,1] + costs[1,2])
th

pred.th = setThreshold(pred, th)
pred.th
pred_data <- pred.th$data
pred_data$valor <- ifelse(pred_data$truth==pred_data$response & pred_data$truth=="BAJA+2",7750,ifelse(pred_data$truth==pred_data$response & pred_data$truth=="CONTINUA",0,ifelse(pred_data$truth!=pred_data$response & pred_data$truth=="BAJA+2",-8000,-250)))

abril_test<- cbind.data.frame(abril_dataset_testing[,1:160],clase_binaria1=abril_dataset_testing$clase_binaria1)
abril_test$participa <- NULL

test.task = makeClassifTask(data = abril_test, target = "clase_binaria1")
test.task = removeConstantFeatures(abril_test)

pred.test = predict(mod, newdata = test.task)
pred.test.th = setThreshold(pred.test, th)
pred.test.th
pred_data <- pred.test.th$data
pred_data$valor <- ifelse(pred_data$truth==pred_data$response & pred_data$truth=="BAJA+2",7750,ifelse(pred_data$truth==pred_data$response & pred_data$truth=="CONTINUA",0,ifelse(pred_data$truth!=pred_data$response & pred_data$truth=="BAJA+2",-8000,-250)))



performance(pred, measures = list(credit.costs, mmce))
performance(pred.th, measures = list(credit.costs, mmce))

## Cross-validated performance with theoretical thresholds
rin = makeResampleInstance("CV", iters = 3, task = credit.task)
lrn = makeLearner("classif.ranger", predict.type = "prob", predict.threshold = th,num.threads=4)
r = resample(lrn, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
r
performance(setThreshold(r$pred, 0.5), measures = list(credit.costs, mmce))
d = generateThreshVsPerfData(r, measures = list(credit.costs, mmce))
plotThreshVsPerf(d, mark.th = th)

tune.res = tuneThreshold(pred = r$pred, measure = credit.costs)
tune.res


df = iris
cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[df$Species,] + runif(150, 0, 10)
colnames(cost) = levels(iris$Species)
rownames(cost) = rownames(iris)
df$Species = NULL
cost



costsens.task = makeCostSensTask(id = "baja", data = df, cost = cost)
costsens.task
lrn = makeLearner("classif.multinom", trace = FALSE)
lrn = makeCostSensWeightedPairsWrapper(lrn)
lrn
mod = train(lrn, costsens.task)
mod
getLearnerModel(mod)
pred = predict(mod, task = costsens.task)
pred
performance(pred, measures = list(meancosts, mcp), task = costsens.task)

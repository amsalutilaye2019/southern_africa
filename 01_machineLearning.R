packages_required <- c("randomForest", "caret", "caretEnsemble", "iml", "tidyverse", 
                       "tidyverse", "elasticnet", "partykit", "rpart", "rpart.plot",
                       "e1071","hydroGOF")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

select <- dplyr::select

cov_data <- read.csv("D:/01CIAT/03Fertilizer/03southernAfricaRecom/data/workspace/trial_covariate.csv",
                     header = T, sep = ",")
dim(cov_data)
colnames(cov_data)

filt_data <- cov_data |> dplyr::select("n_kg_ha", "p_kg_ha", "k_kg_ha", 
                                       "variety", "grain_kg_ha", "al_0.20cm", 
                                       "bdr_0.200cm", "clay.tot.psa_0.20cm", 
                                       "c.tot.0.20cm", "ca_0.20cm", "db.od_0.20cm", 
                                       "ecec.f_0.20cm", "fe_0.20cm", "k_0.20cm", 
                                       "mg_0.20cm", "n.tot.ncs_0.20cm", "oc_0.20cm", 
                                       "p_0.20cm", "ph.h2o_0.20cm", "sand.tot.psa_0.20cm", 
                                       "silt.tot.psa_0.20cm", "s_0.20cm", 
                                       "texture.class_0.20cm", "wpg2_0.20cm", 
                                       "zn_0.20cm", "tmax_1", "tmax_2", "tmax_3", 
                                       "tmax_4", "tmax_12", "tmin_1", "tmin_2", 
                                       "tmin_3", "tmin_4", "tmin_12", "tavg_1", 
                                       "tavg_2", "tavg_3", "tavg_4", "tavg_12", 
                                       "prec_1", "prec_2", "prec_3", "prec_4", 
                                       "prec_12", "srad_1", "srad_2", "srad_3", 
                                       "srad_4", "srad_12", "wind_1", "wind_2", 
                                       "wind_3", "wind_4", "wind_12", "elevation", 
                                       "slope", "TPI", "TRI") |> na.omit() |>
                                        unique()
dim(filt_data)

#convert categorical classes as factors
filt_data$variety <- as.factor(filt_data$variety)
filt_data$texture.class_0.20cm <- as.factor(filt_data$texture.class_0.20cm)

#let's check the distribution of the data
hist(filt_data$grain_kg_ha)
skewness_value <- skewness(filt_data$grain_kg_ha)
print(skewness_value)

# partitioning data into train and test set
set.seed(123)
train_seq <- createDataPartition(filt_data$grain_kg_ha, p=0.9, list=F)
train_data <- filt_data[train_seq,]
test_data <- filt_data[-train_seq,]
#
# check near zero variance predictor
nzv <- nearZeroVar(train_data, saveMetrics=T)
nzv

#based on near zero variance, wind_4 and bdr_0.200cm should be removed from the 
#data

train_data <- train_data |> select(-c(wind_4, bdr_0.200cm))
test_data <- test_data |> select(-c(wind_4, bdr_0.200cm))

# five-fold cross-validation repeated once with random sampling
control <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    search = "random"
  )
algorithmList <- c("ranger", "gbm",  'xgbTree', 'svmRadial')
models <- caretList(grain_kg_ha ~ ., data=train_data, trControl=control, methodList=algorithmList)
model_selection <- summary(resamples(models))
model_selection

#RFE
# define five-fold cross-validation scheme
control_feature <- rfeControl(functions=rfFuncs, method="cv", number=5)
#
# recursive feature elimination; 'sizes' stands for the number of variables
feature_selection <- rfe(train_data[,c(1:4, 6:57)], train_data[,5], sizes=c(1:57), rfeControl=control_feature)
#
# rmse vs. number of variables (ordered from very to not important)
plot(feature_selection)

#select the optimal variables
# list all variables
list_of_var <- feature_selection$optVariables
list_of_var <- c("grain_kg_ha", "n_kg_ha", "p_kg_ha", list_of_var)
#
# select 10 most important variables only
train_data <- train_data[,list_of_var]
test_data <- test_data[,list_of_var]

#hyper parameter tuning
set.seed(123)
mtry <- as.integer((ncol(train_data))/3) #this will be optimized
mtry <- seq(mtry - 8, mtry + 8, by = 2)
mtry <- mtry[mtry > 0]

rf_fitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5)

rf_tuneGrid <- expand.grid(.mtry = mtry,
                           .splitrule =  "maxstat",
                           .min.node.size = c(20, 30))

mod_fit <- train(
  grain_kg_ha ~ .,
  data = train_data,
  method = "ranger",
  trControl = rf_fitControl,
  importance = 'impurity',
  tuneGrid = rf_tuneGrid,
  preProcess = c('scale', 'center'))

params <-  max(mod_fit$results$Rsquared)
best_parames <- mod_fit$results |> dplyr::filter(Rsquared == params)

# set final hyper-parameter values
ntrees_model <- 500
mtry <- best_parames$mtry
nodesize_model <- best_parames$min.node.size
tuneGrid_model <- expand.grid(.mtry = mtry)
#
# define cross-validation scheme
control_model <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 5)
#
# fit the final random forest model
rf_model_final <- train(
  grain_kg_ha ~ .,
  data = train_data,
  method = "rf",
  tuneGrid = tuneGrid_model,
  trControl = control_model,
  ntree = ntrees_model,
  nodesize = nodesize_model
)


imp <- varImp(rf_model_final)
ggplot(imp)

pred <- predict(mod_fit, test_data)
gof <- ggof(pred, test_data$grain_kg_ha)

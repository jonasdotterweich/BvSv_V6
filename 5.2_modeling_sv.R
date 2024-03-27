##################### Now Snow Vole Model  ############################

library(flexsdm)
require(dplyr)


#loading model dataframe

df_SV <- read.csv("data_to_model/df_SV+bivars.csv")


#splitting the data frame into training and testing set
#indexes of the data.frame (80% of it)
set.seed(187)

ind <- sample(1:nrow(df_SV), 0.8*nrow(df_SV))

train <- df_SV[ind,]

test <- df_SV[-ind,]

write.csv(train, "data_to_model/training_SV_V6.csv")

write.csv(test, "data_to_model/testing_SV_V6.csv")




#### now models with the training set:

#creating the partition, which can be used to train a series of models

part_train <- part_random(
  data = train,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))




#### GBM fit ####

GBM_fit1 <- fit_gbm(
  part_train,
  response = "pr_ab",
  predictors= c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
  partition = ".part",
  thr= c("max_sens_spec"))


perf_result_gbm <- GBM_fit1$performance




#### RAF Fit ####

RAF_fit1 <- fit_raf(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_raf <- RAF_fit1$performance




#### SVM fit ####

SVM_fit1 <- fit_svm(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_svm <- SVM_fit1$performance




##### Tuning of models #### 

##gbm ###

#grid for  GBM model tuning

tune_grid_gbm <- expand.grid(n.trees = c(20, 50, 100),
                             shrinkage = c(0.1, 0.5, 1),
                             n.minobsinnode = c(1, 3, 5, 7, 9)
)



gbm_tune <-
  tune_gbm(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
    partition = ".part",
    grid = tune_grid_gbm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )




hyper_gbmtune <- gbm_tune$hyper_performance



### random forest

tune_grid_raf <-
  expand.grid(mtry = seq(1, 7, 1))

rf_tune <-
  tune_raf(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
    partition = ".part",
    grid = tune_grid_raf,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_rftune <- rf_tune$hyper_performance





### svm tune ###

tune_grid_svm <-
  expand.grid(
    C = c(2, 4, 8, 16, 20),
    sigma = c(0.01, 0.1, 0.2, 0.3, 0.4)
  )

svm_tune <-
  tune_svm(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
    partition = ".part",
    grid = tune_grid_svm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

hyper_svmtune <- svm_tune$hyper_performance



### GBM 2

hyper_gbmtune[which(hyper_gbmtune$TSS_mean == max(hyper_gbmtune$TSS_mean)),]

#tree:100
#shrink:0.5
#minobs: 3

GBM_fit2 <- fit_gbm(
  part_train,
  response = "pr_ab",
  predictors= c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
  partition = ".part",
  thr= c("max_sens_spec"),
  n_trees = 100,
  n_minobsinnode = 3,
  shrinkage = 0.5
)

perf_res_gbm2 <- GBM_fit2$performance

perf_res_gbm2$TSS_mean
perf_result_gbm$TSS_mean



### Random forest

hyper_rftune[which(hyper_rftune$TSS_mean == max(hyper_rftune$TSS_mean)),]

#

RAF_fit2 <- fit_raf(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    mtry = 7
)


perf_res_rf2 <- RAF_fit2$performance

perf_res_rf2$TSS_mean
perf_result_raf$TSS_mean




###SVM 


hyper_svmtune[which(hyper_svmtune$TSS_mean == max(hyper_svmtune$TSS_mean)),]

#c=2
#sigma=0.1

SVM_fit2 <- fit_svm(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    sigma = 0.1,
                    C = 2
)

perf_res_svm2 <- SVM_fit2$performance

perf_res_svm2$TSS_mean
perf_result_svm$TSS_mean




####### Ensemble ##### 


SV_ensembled_v6 <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

perf_ensemble <- SV_ensembled_v6$performance





#### out of interest trying to model maxent as wel, I do remember that something dint work with it in combiantion. However I am interested

### adding background points for the MAX

backSV <- read.csv("data_to_model/backSV.csv")



part_back_sv <- part_random(
  data = backSV,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))




MAX_fit1 <- fit_max(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15" ),
                    partition = ".part",
                    background = part_back_sv,
                    thr = c("max_sens_spec")
)

perf_result_max <- MAX_fit1$performance


### doesnt seem bad, will tune it and see how it fairs in ensemble model

tune_grid_max <-
  expand.grid(
    regmult = seq(0.1, 3, 0.5),
    classes = c("l", "lq", "lqh")
  )


max_tune <-
  tune_max(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15" ),
    partition = ".part",
    background = part_back_sv,
    grid = tune_grid_max,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_maxtune <- max_tune$hyper_performance

hyper_maxtune[which(hyper_maxtune$TSS_mean == max(hyper_maxtune$TSS_mean)),]


MAX_fit2 <- fit_max(part_train,
                    response = "pr_ab",
                    predictors = c("BIO3",                    "BIO4",                     "BIO14",                    "BIO15" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite", "climaticMoistureIndex",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETWettestQuarter" ,       "TRI",                     "WC_alt_lonlat" ,           "mergedLC_15" ),
                    partition = ".part",
                    background = part_back_sv,
                    thr = c("max_sens_spec"),
                    clamp = TRUE,
                    classes = "lqh",
                    pred_type = "cloglog",
                    regmult = 0.6
)

perf_res_max2 <- MAX_fit2$performance

perf_res_max2$TSS_mean
perf_result_max$TSS_mean



### now lets see what performace we get in the ensemble



SV_ensembled_v6.3 <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit2, MAX_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
)

perf_ensemble_3 <- SV_ensembled_v6.3$performance


#### ere we obtaines somehow slightly worse performance but overall still well

### better performance then without, sehr gut

write.csv(perf_ensemble_3, "models/perf_ensemble_SV_V6.3.csv")

save(SV_ensembled_v6.3, file = "models/SV_ensembled_V6.3.Rda")





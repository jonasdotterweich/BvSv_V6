#### checking the variable importances


library(vip)
library(rminer)
library(dplyr)
library(SDMtune)
library(fitMaxnet)


#loading the model for BV

load("models/BV_ensembled_V6.3.Rda")

#loading the model for SV

load("models/SV_ensembled_V6.3.Rda")







BVperf <- BV_ensembled_v6.3$performance

BVmodels <- BV_ensembled_v6.3$models

BV_GBM <- BVmodels$m_1
BV_RAF <- BVmodels$m_2
BV_SVM <- BVmodels$m_3
BV_MAX <- BVmodels$m_4


BV_GBM_m <-BV_GBM$model
BV_RAF_m <-BV_RAF$model
BV_SVM_m <-BV_SVM$model
BV_MAX_m <-BV_MAX$model



vip_gbmBV <- vip(BV_GBM_m, num_features = 13)

vip_rafBV <- vip(BV_RAF_m, num_features = 13)

vip_gbmBV_df <- vip_gbmBV$data

vip_rafBV_df <- vip_rafBV$data












### from here, let the hassle start with the varimps for maxnet


vip_maxBV <- varImportance(BV_MAX_m)

## forwhat I need now is the train df

trainBV <- read_csv("data_to_model/training_BV_V6.csv")

### need to split it up into presences and absences for background data 


occBV_bivars <- trainBV %>% filter(pr_ab ==1)
bckgBV_bivars <- trainBV %>% filter(pr_ab ==0)

vip_maxBV <- varImportance(BV_MAX_m,
                           occBV_bivars, 
                           bckgBV_bivars)


### waht needs to be done now is to find a package wich can handle the variable importances of the maxnet model

#### trying with caret

caret::varImp(BV_MAX_m, newdata = envData, lambda = 1, type = "coef"  )

predict.maxnet

train_vect <- as.vector.data.frame(trainBV)

####ending with caret as I dont know how to pass newdata


### trying to calculate them "by hand" with advise from the friendly GPT

coefficients <- BV_MAX_m$beta

# Calculate variable importance
variable_importance <- abs(coefficients)

# Normalize variable importances
normalized_importance <- variable_importance / sum(variable_importance)

# Now you have the normalized variable importances

### somehow I didn't achive a usable result form these calculations reasoning taht it turns out nNA...

### trying it like Peter Bat

varList <- names(BV_MAX_m$samplemeans)

importance <- vector("numeric", length(varList))
names(importance) <- varList

envData <- rbind(occBV_bivars[, -c(1:3)], bckgBV_bivars[, -c(1:3)])

fullModelVals <- predict(BV_MAX_m, envData)

predict(BV_MAX_m)


class(occBV_bivars)


### an error could be that the data I am passing the function is not in the correct SWD format tht it wants it to be
# and with data I mean the dataframe with p_ab and the bivars

## therefore I am installing a package hich lets me create these dataframes easily 

library(remotes)
remotes::install_github("luismurao/ntbox")

library(ntbox)

### okay now trying to create a SWD object

## loading what is needed

# env_layers -> bivars vifed

env_layers <- rast("biovars/biovars_VIFed_V6.tif")

env_layers <- brick(env_layers)

no_bkg <- read_csv("data_to_model/backBV.csv")



BVname <- "Myodes glareolus"


occ_SWD <- swd_format(env_layers = env_layers,
                      nbg = no_bkg,
                      occs_points = occBV_bivars,
                      sp_name = BVname,
                      longitude = "decimalLongitude",
                      latitude = "decimalLatitude")



## trying to put it as sole df and trying the varimp again

occBV_bio_df <- as.data.frame(occBV_bivars)

class(occBV_bio_df)

no_bkg_df <- as.data.frame(no_bkg)

vip_maxBV <- varImportance(BV_MAX_m,
                           occBV_bio_df, 
                           no_bkg_df)

#### wow, with that the function works but I have the same problem as I had before in doing it step by step
## I have the issue with the NA values
## Now the question remains, why is it lik that


rminer::Importance(BV_MAX_m, trainBV)

class(BV_MAX_m)




coefficients <- BV_MAX_m$beta

# Find the index of lambda.min (or your chosen lambda) in the lambda sequence
lambda_index <- which.min(BV_MAX_m$lambda)


# Extract coefficients for this specific lambda
selected_coefficients <- BV_MAX_m$beta[, lambda_index]

# Convert the sparse matrix to a regular vector (if necessary)
coefficients_vector <- as.vector(selected_coefficients)

# You can then view or process these coefficients
print(coefficients_vector)





##### even after inclusion of backgroundpoints created for model it is still na present for the importances

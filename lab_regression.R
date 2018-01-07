#################################################################################
##############              Lab Regression     ############################
##############     ----------- solution ---------   ############################
#################################################################################

rm(list = ls())
library(caret)
library(ggplot2)
library(GGally)
library(leaps)
library(glmnet)
library(pls)
library(car)
library(corrplot)
## Load functions --------------------------------------------------------------------------------
#Source plotModelDiagnosis function, a custom function useful for diagnosis the adjustment of a model.
source("plotModelDiagnosis.R")

##
setwd("~/Desktop/MBD.comillas/ML1/practicas/regresion")

## LoadData 
fdata <- read.csv(file = "SP_ElectMarket_H.csv" , sep=";" )

fdata<-fdata[,1:11]
fdata<-fdata[1000:2001,]

# Analisis exploratorio
sapply(X = fdata , function(x) sum(is.na(x))) # fdata = na.omit(fdata) #Eliminate NA
str(fdata)
summary(fdata)

fdata$Y<-sqrt(abs(fdata$HYDR_FR*fdata$DEM_SP-fdata$NUCL_SP*fdata$DEM_FR))


# ggpairs(fdata,aes(alpha = 0.3))

#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "circle")

## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Y,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR,       #split probability
                                  list = FALSE,      #Avoid output as a list
                                  times = 1)         #only one partition
#obtain training and validation sets
fdata_train = fdata[trainIndex,]
fdata_val = fdata[-trainIndex,]

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## Linear Regression -------------------------------------------------------------------------------------------
set.seed(150) #For replication
lm.fit = train(fdata_train[,varindex], 
               y = fdata_train$Salary, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit)  #information about the model trained
#Plot results of training
par(mfrow =c(2,2))
plot(lm.fit$finalModel)
par(mfrow = c(1,1))
#Identify correlated variables
vif(lm.fit$finalModel)



## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## Linear Regression -------------------------------------------------------------------------------------------
set.seed(150) #For replication
lm.fit = train(fdata_train[,1:11], 
               y = fdata_train$charges, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit)  #information about the model trained
#Plot results of training
par(mfrow =c(2,2))
plot(lm.fit$finalModel)
par(mfrow = c(1,1))
#Identify correlated variables
vif(lm.fit$finalModel)


#Evaluate the model with training sets and diagnosis
fdataTR_eval = fdata_train
fdataTR_eval$lm_pred = predict(lm.fit,  newdata = fdata_train)
fdataTR_eval$residuo <- fdataTR_eval$charges - fdataTR_eval$lm_pred


plot(x = fdataTR_eval$charges , y = fdataTR_eval$residuo)

source("plotModelDiagnosis.R")
plotModelDiagnosis(fdata_train[,varindex], fdata_train$charges, fdataTR_eval$lm_pred)
ggplot(fdataTR_eval)+ geom_point(aes(x=lm_pred,y=residuo,color=smoker))
ggplot(fdataTR_eval)+ geom_point(aes(x=lm_pred,y=residuo,color=region))
ggplot(fdataTR_eval)+ geom_point(aes(x=lm_pred,y=residuo,color=sex))
ggplot(fdataTR_eval)+ geom_point(aes(x=lm_pred,y=residuo,color=smoker))


#-------------------------------------------------------------------------------------------------
#-----------------------------------  Dibujar las variables   ------------------------------------
#-------------------------------------------------------------------------------------------------

predict(lm.fit,  newdata = fdata_train)

ggplot(dataplot)+ geom_col(aes(x=x,y=y))


#-------------------------------------------------------------------------------------------------
#----------------------------------- VARIABLE SELECTION   ------------------------------------------------
#-------------------------------------------------------------------------------------------------
###best subset selection
#Load customized model from IIT
load("lmExhaustive.Rdata")
set.seed(150) #For replication

#With categorical variables, formula method should be used
lm_best.fit = train(form = charges~.,
                    data = fdata_train, 
                    method = lmExhaustive,
                    tuneGrid = data.frame(nvmax = 6),
                    preProcess = c("center","scale"),
                    trControl = ctrl_tune, 
                    metric = "RMSE")
lm_best.fit #information about the resampling
summary(lm_best.fit$finalModel) #information about the fitted model
lm_best.summary <- summary(lm_best.fit$finalModel) #Obtain summary information for plotting
par(mfrow =c(2,2))
#Residuals vs n_vars plot 
plot(lm_best.summary$rss ,xlab="Number of Variables",ylab=" RSS", type="l")
#adjR2 vs n_vars plot 
plot(lm_best.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")              
isel <- which.max (lm_best.summary$adjr2)
points (isel, lm_best.summary$adjr2[isel], col ="red",cex =2, pch =20)
#cp vs n_vars plot 
plot(lm_best.summary$cp ,xlab =" Number of Variables ",ylab="Cp",   type="l")
isel <-which.min (lm_best.summary$cp )
points (isel, lm_best.summary$cp [isel], col ="red",cex =2, pch =20)
#bic vs n_vars plot 
isel <-which.min (lm_best.summary$bic )
plot(lm_best.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
points (isel, lm_best.summary$bic [isel], col =" red",cex =2, pch =20)
par(mfrow =c(1,1))

###forward variable selection
set.seed(150) #For replication
#With categorical variables, formula method should be used
lm_forward.fit = train(form = charges~.,
                       data = fdata_train, 
                       method = "leapForward",
                       tuneGrid = data.frame(nvmax = 20),
                       preProcess = c("center","scale"),
                       trControl = ctrl_tune, 
                       metric = "RMSE")
lm_forward.fit #information about the resampling
summary(lm_forward.fit$finalModel)
lm_forward.summary <- summary(lm_forward.fit$finalModel)
par(mfrow =c(2,2))
#Residuals vs n_vars plot 
plot(lm_forward.summary$rss ,xlab="Number of Variables",ylab=" RSS", type="l")
#adjR2 vs n_vars plot 
plot(lm_forward.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")              
isel <- which.max (lm_forward.summary$adjr2)
points (isel, lm_forward.summary$adjr2[isel], col ="red",cex =2, pch =20)
#cp vs n_vars plot 
plot(lm_forward.summary$cp ,xlab =" Number of Variables ",ylab="Cp",   type="l")
isel <-which.min (lm_forward.summary$cp )
points (isel, lm_forward.summary$cp [isel], col ="red",cex =2, pch =20)
#bic vs n_vars plot 
isel <-which.min (lm_forward.summary$bic )
plot(lm_forward.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
points (isel, lm_forward.summary$bic [isel], col =" red",cex =2, pch =20)
par(mfrow =c(1,1))

###backward variable selection
set.seed(150) #For replication
#With categorical variables, formula method should be used
lm_backward.fit = train(form = charges~.,
                        data = fdata_train, 
                        method = "leapBackward",
                        tuneGrid = data.frame(nvmax = 20),
                        preProcess = c("center","scale"),
                        trControl = ctrl_tune, 
                        metric = "RMSE")
lm_backward.fit #information about the resampling
summary(lm_backward.fit$finalModel)
lm_backward.summary <- summary(lm_backward.fit$finalModel)
par(mfrow =c(2,2))
#Residuals vs n_vars plot 
plot(lm_backward.summary$rss ,xlab="Number of Variables",ylab=" RSS", type="l")
#adjR2 vs n_vars plot 
plot(lm_backward.summary$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type="l")              
isel <- which.max (lm_backward.summary$adjr2)
points (isel, lm_backward.summary$adjr2[isel], col ="red",cex =2, pch =20)
#cp vs n_vars plot 
plot(lm_backward.summary$cp ,xlab =" Number of Variables ",ylab="Cp",   type="l")
isel <-which.min (lm_backward.summary$cp )
points (isel, lm_backward.summary$cp [isel], col ="red",cex =2, pch =20)
#bic vs n_vars plot 
isel <-which.min (lm_backward.summary$bic )
plot(lm_backward.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
points (isel, lm_backward.summary$bic [isel], col =" red",cex =2, pch =20)
par(mfrow =c(1,1))

#-------------------------------------------------------------------------------------------------
#----------------------------------- Regularized method   ------------------------------------------------
#-------------------------------------------------------------------------------------------------

#######Ridge regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
ridge.fit = train(form = charges~.,
                  data = fdata_train, 
                  method = "glmnet",
                  tuneGrid = expand.grid(
                    lambda = 10^ seq(-2,6, length =20), #Lambda cannot be greater than 10^6
                    alpha = 0),
                  preProcess = c("center","scale"),
                  trControl = ctrl_tune, 
                  metric = "RMSE")
ridge.fit #information about the resampling
ggplot(ridge.fit)+scale_x_log10()
#Plot the evolution of the coefficients as a function of lambda
plot(ridge.fit$finalModel)
#Coefs for high value of lambda
ridge.fit$finalModel$lambda[5]
coef(ridge.fit$finalModel)[,5]
#Coefs for low value of lambda
ridge.fit$finalModel$lambda[95]
coef(ridge.fit$finalModel)[,95]




#######lasso regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
lasso.fit = train(form = charges~.,
                  data = fdata_train, 
                  method = "glmnet",
                  tuneGrid = expand.grid(
                    lambda = 2*10^seq(-2,2, length =20), #Lambda cannot be greater than 260
                    alpha = 1),
                  preProcess = c("center","scale"),
                  trControl = ctrl_tune, 
                  metric = "RMSE")
lasso.fit #information about the resampling
ggplot(lasso.fit)+scale_x_log10()
#Plot the evolution of the coefficients as a function of lambda
plot(lasso.fit$finalModel)
#Coefs for high value of lambda
lasso.fit$finalModel$lambda[5]
coef(lasso.fit$finalModel)[,5]
#Coefs for low value of lambda
lasso.fit$finalModel$lambda[60]
coef(lasso.fit$finalModel)[,60]


#-------------------------------------------------------------------------------------------------
#--------------------------- Dimension reduction method ------------------------------------------
#-------------------------------------------------------------------------------------------------


#######Principal component regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
pcr.fit = train(form = charges~.,
                data = fdata_train, 
                method = "pcr",
                tuneGrid = data.frame(ncomp = 1:6),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
pcr.fit #information about the resampling
ggplot(pcr.fit)
summary(pcr.fit$finalModel)
plot(pcr.fit$finalModel$projection)
pcr.fit$finalModel$loadings
#plot loadings of component 1
dataplot = data.frame(x = rownames(pcr.fit$finalModel$loadings), y =pcr.fit$finalModel$loadings[,1])
ggplot(dataplot)+ geom_col(aes(x=x,y=y))



#######Partial least squares regression
set.seed(150) #For replication
#With categorical variables, formula method should be used
plsr.fit = train(form = charges~.,
                 data = fdata_train, 
                 method = "pls",
                 tuneGrid = data.frame(ncomp = 1:6),
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
plsr.fit #information about the resampling
ggplot(plsr.fit)+scale_x_log10()
summary(plsr.fit$finalModel)



transformResults <- resamples(list(
  lm=lm.fit,
  lm_backward=lm_backward.fit,
  lm_forward=lm_forward.fit,
  ridge = ridge.fit,
  lasso = lasso.fit,
  pcr = pcr.fit,
  pls = plsr.fit))
summary(transformResults)
dotplot(transformResults)



#################################################################################################
###################### Validation Results #######################################################
#################################################################################################

#validation
fdataTV_eval = fdata_val
fdataTV_eval$lm_pred = predict(lm.fit,  newdata = fdata_val) 

## compare results -------------------------------------------------------------------------------
caret::R2(fdataTR_eval$lm_pred,fdataTR_eval$charges)
caret::R2(fdataTV_eval$lm_pred,fdataTV_eval$charges)

caret::RMSE(fdataTR_eval$lm_pred,fdataTR_eval$charges)
caret::RMSE(fdataTV_eval$lm_pred,fdataTV_eval$charges)

#-------------------------------------------------------------------------------------------------
#-------------------------------- Polynomial regression ------------------------------------------
#-------------------------------------------------------------------------------------------------
#Create new variables
fdata_train_poly <- fdata_train
fdata_train_poly$Years2 <- fdata_train_poly$Years^2
set.seed(150) #For replication
poly.fit = train(form = charges~.,
                 data = fdata_train_poly, 
                 method = "lm", #Linear model
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
poly.fit #information about the resampling settings
summary(poly.fit)  #information about the model trained

#Evaluate the polynomial model with training sets and diagnosis
fdata_train_poly$lm_pred = predict(poly.fit,  newdata = fdata_train_poly)  

# ggplot(fdata_train_poly)+geom_point(aes(x=Years, y = Salary-lm_pred))


####### OTHER OPTION
#Other option is to adapt the formula in the train function:
#2-degree polynomial for Years 
#form = Salary~poly(Years,2)

#2-degree polynomial for Years 
#form = Salary~cbind(Years, Years^2)



#-------------------------------------------------------------------------------------------------
#----------------------- Smooth splines   ------------------------------------------------
#-------------------------------------------------------------------------------------------------

set.seed(150) #For replication
Smooth.fit = train(form = charges~age,
                   data = fdata_train, 
                   method = "gamSpline",
                   tuneGrid = data.frame(df = seq(2,10,2)),
                   preProcess = c("center","scale"),
                   trControl = ctrl_tune, 
                   metric = "RMSE")
Smooth.fit #information about the resampling settings
ggplot(Smooth.fit)
summary(Smooth.fit)  #information about the model trained
#Plot the fitted splines 
#Careful, if library "car" is loaded, this plot gives an error
gam::plot.gam(Smooth.fit$finalModel, se=TRUE, residuals=TRUE)




#-------------------------------------------------------------------------------------------------
#----------------------- GAM with splines   ------------------------------------------------
#-------------------------------------------------------------------------------------------------

set.seed(150) #For replication
gam.fit = train(form = charges~.,
                data = fdata_train, 
                method = "gamSpline",
                tuneGrid = data.frame(df = seq(2,10,4)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
gam::plot.gam(gam.fit$finalModel, se=TRUE ,col ="blue ")






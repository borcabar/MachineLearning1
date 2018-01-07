#Apuntes de preparacion apra el exámen de ML1 del 11 de Diciembre 2017

#-----------------------------   load librtaries    -------------------------

library(caret)
library(ggplot2)
library(pROC)
library(NeuralNetTools) ##Useful tools for plotting and analyzing neural networks
library(nnet)
library(GGally)
library(gam)
library(splines)
library(corrplot)

load("svmRadialSigmaIIT.rdata")





#----------------------------------------------------------------------------






## Load functions --------------------------------------------------------------------------------
#Source plot2Dclass function, a custom function useful for plotting the results of classification models 
#with two input variables and one output.
source("plot2Dclass.R")




## Load and processing of the filefile -------------------------------------------------------------------------------------
fdata = read.table("PBCb.dat", sep = "", header = TRUE, stringsAsFactors = TRUE)
str(fdata)

#en caso de necesitar
fdata$Y = as.factor(fdata$Y) 

colnames(fdata)[4] <- "Y"
fdata <- na.omit(fdata)

fdata$X1<-fdata$X1/mean(fdata$X1)
fdata$X2<-fdata$X2/mean(fdata$X1)


#---------------------------------      Explore the data       ----------------------------------


ggplot(data = fdata) + geom_point(mapping = aes(x = displ, y = hwy, size = cyl, color = class))
ggplot(fdata)+geom_point(aes(x=X1,y=X3,color=Y))
ggplot(fdata)+geom_point(aes(x=X1,y=X2,color=Y))
ggpairs(fdata,aes( alpha = 0.3))
ggpairs(fdata , aes(color = Y , alpha = 0.5))
fdata <- fdata[c(1,2,4)]
levels(fdata$Y)=c('NO','YES')


#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "circle")



## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Y,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fdata_train = fdata[trainIndex,]
fdata_val = fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
#There is not parameter tuning in LDA or QDA, therefore, no cross-validation method is used
ctrl <- trainControl(method = "none",                      # method= "none" when no resampling is used
                     summaryFunction = twoClassSummary,    #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE,                    #Compute class probs in Hold-out samples
                     returnResamp = "all",                 #Return all information about resampling
                     savePredictions = TRUE)               #save predictions

## Decision trees -------------------------------------------------------------------------------------------
set.seed(150) #For replication
#first, with fixed parameters
#Train model using training data
tree.fit = train(fdata_train[,c("X1","X2")], #Input variables. Other option: fdata[,1:2]
                 y = fdata_train$Y, 
                 method = "rpart",
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(cp = 0.1),
                 trControl = ctrl, 
                 metric = "ROC")
tree.fit #information about the resampling settings
summary(tree.fit)  #information about the model trained
tree.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
#Plot the tree:
plot(tree.fit$finalModel, uniform=TRUE,margin=0.2)
text(tree.fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and validation sets
#training
fdataTR_eval = fdata_train
fdataTR_eval$tree_prob = predict(tree.fit, type="prob" , newdata = fdata_train) # predict probabilities
fdataTR_eval$tree_pred = predict(tree.fit, type="raw" , newdata = fdata_train) # predict classes 
#validation
fdataTV_eval = fdata_val
fdataTV_eval$tree_prob = predict(tree.fit, type="prob" , newdata = fdata_val) # predict probabilities
fdataTV_eval$tree_pred = predict(tree.fit, type="raw" , newdata = fdata_val) # predict classes 


#Plot predictions of the model
ggplot(fdataTR_eval)+geom_point(aes(x=X1,y=X2,color=tree_pred))
ggplot(fdataTV_eval)+geom_point(aes(x=X1,y=X2,color=tree_pred))

## compare results -------------------------------------------------------------------------------
#confusion matices
confusionMatrix(fdataTR_eval$tree_pred, fdataTR_eval$Y)
confusionMatrix(fdataTV_eval$tree_pred, fdataTV_eval$Y)

#calibration plot
#Use cuts for setting the number of probability splits
#training
calPlotData <- calibration(Y ~ tree_prob$YES, data = fdataTR_eval, class = "YES",cuts = 6) 
xyplot(calPlotData, auto.key = list(columns = 2))
#validation
calPlotData <- calibration(Y ~ tree_prob$YES, data =fdataTV_eval, class = "YES",cuts = 6)
xyplot(calPlotData, auto.key = list(columns = 2))

# ROC curve
#training
reducedRoc <- roc(response = fdataTR_eval$Y, fdataTR_eval$tree_prob$YES)
plot(reducedRoc)
auc(reducedRoc)
#validation
reducedRoc <- roc(response = fdataTV_eval$Y, fdataTV_eval$tree_prob$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)

ctrl_tune <- trainControl(method = "cv",
                          number = 10,
                          p = 0.7, #Used in method LGOCV
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,                    #Compute class probs in Hold-out samples
                          returnResamp = "all",                 #Return all information about resampling
                          savePredictions = TRUE)               #save predictions

#############Train model using the desired resamling method
###The complexity parameter is chosen between the values given in tuneGrid
set.seed(150) #For replication
tree.fit = train(fdata_train[,c("X1","X2")], 
                 y = fdata_train$Y, 
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0,0.4,0.05)),
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "ROC")
tree.fit #information about the resampling
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
str(tree.fit$resample) #resampling metrics


#--------------------------------------------------------------------------------
#------------------------    SVM radial    ----------------------------------
#--------------------------------------------------------------------------------

set.seed(150) #For replication
#first, with fixed parameters
#Train model using training data
svm.fit = train(fdata_train[,c("X1","X2")], #Input variables. Other option: fdata[,1:2]
                y = fdata_train$Y, 
                method = svmRadialSigmaIIT,
                preProcess = c("center","scale"),
                tuneGrid =  data.frame( sigma=0.01, C=25),  
                trControl = ctrl_tune, 
                metric = "ROC")
svm.fit #information about the resampling settings
svm.fit$finalModel #information about the model trained
#Plot the svm support vectors:
isupvect <- alphaindex(svm.fit$finalModel)[[1]] #indexes for support vectors
#plot support vectors
ggplot()+geom_point(data = fdata_train[isupvect,],aes(x = X1, y = X2),color="red")+
  geom_point(data = fdata_train[-isupvect,],aes(x = X1, y = X2))

## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and validation sets
#training
fdataTR_eval = fdata_train
fdataTR_eval$svm_prob = predict(svm.fit, type="prob" , newdata = fdata_train[,1:2]) # predict probabilities
fdataTR_eval$svm_pred = predict(svm.fit, type="raw" , newdata = fdata_train[,1:2]) # predict classes 
#validation
fdataTV_eval = fdata_val
fdataTV_eval$svm_prob = predict(svm.fit, type="prob" , newdata = fdata_val[,1:2]) # predict probabilities
fdataTV_eval$svm_pred = predict(svm.fit, type="raw" , newdata = fdata_val[1:2]) # predict classes 


#Plot predictions of the model
ggplot(fdataTR_eval)+geom_point(aes(x=X1,y=X2,color=svm_pred))
ggplot(fdataTV_eval)+geom_point(aes(x=X1,y=X2,color=svm_pred))


## Plot a 2D graph with the results of the model -------------------------------------------------
# Grid for evaluating the model 
#The following lines create a grid of points
np_grid <- 300 #number of discretization points in each dimension
np.X1 <- seq(from = min(fdata$X1), to = max(fdata$X1), length.out = np_grid)
np.X2 <- seq(from = min(fdata$X2), to = max(fdata$X2), length.out = np_grid)
grid_X1_X2 <- expand.grid(X1 = np.X1, X2 = np.X2)

#predict each point of the grid and plot the result
grid_X1_X2$pred = predict(svm.fit, type="raw" , newdata = grid_X1_X2) # predicted probabilities for class YES
ggplot(grid_X1_X2)+geom_point(aes(x=X1,y=X2,color=pred))


## Plot a 2D graph with the results of the model -------------------------------------------------
#Obtain probabilites of the model in the grid and add to the grid data frame
probGrid = predict(svm.fit, type="prob" , newdata = grid_X1_X2[,c("X1","X2")]) # predict probabilities
grid_X1_X2$prob = probGrid$YES
#plot2Dclass
plot2Dclass(fdataTR_eval,grid_X1_X2,fdataTR_eval$svm_pred)
plot2Dclass(fdataTV_eval,grid_X1_X2,fdataTV_eval$svm_pred)

#plot the probailities of the grid
ggplot(grid_X1_X2)+geom_point(aes(x=X1,y=X2,color=prob))


## compare results -------------------------------------------------------------------------------
#confusion matices
confusionMatrix(fdataTR_eval$svm_pred, fdataTR_eval$Y)
confusionMatrix(fdataTV_eval$svm_pred, fdataTV_eval$Y)

#Histogram comparison
#train
ggplot(fdataTR_eval) + 
  geom_histogram(aes(x = svm_prob$YES, fill = Y), bins = 15) + 
  facet_wrap(~Y) + 
  labs(x="Probability of Class YES", title = "Histogram for training data")
#validation
ggplot(fdataTV_eval) + 
  geom_histogram(aes(x = svm_prob$YES, fill = Y), bins = 15) + 
  facet_wrap(~Y) + 
  labs(x="Probability of Class YES", title = "Histogram for validation data")

#calibration plot
#Use cuts for setting the number of probability splits
#training
calPlotData <- calibration(Y ~ svm_prob$YES, data = fdataTR_eval, class = "YES",cuts = 6) 
xyplot(calPlotData, auto.key = list(columns = 2))
#validation
calPlotData <- calibration(Y ~ svm_prob$YES, data =fdataTV_eval, class = "YES",cuts = 6)
xyplot(calPlotData, auto.key = list(columns = 2))


# ROC curve
#training
reducedRoc <- roc(response = fdataTR_eval$Y, fdataTR_eval$svm_prob$YES)
plot(reducedRoc)
auc(reducedRoc)
#validation
reducedRoc <- roc(response = fdataTV_eval$Y, fdataTV_eval$svm_prob$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)






##Repeated train-test splits with 70% train and 30% test. 10 different splits.
ctrl_tune <- trainControl(method = "cv",
                          number = 10,
                          p = 0.7, #Used in method LGOCV
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,                    #Compute class probs in Hold-out samples
                          returnResamp = "all",                 #Return all information about resampling
                          savePredictions = TRUE)               #save predictions

#############Train model using the desired resampling method
###The cost value is chosen between the values given in tuneGrid
set.seed(150) #For replication
svm.fit = train(fdata_train[,c("X1","X2")], 
                y = fdata_train$Y, 
                method = svmRadialSigmaIIT,
                tuneGrid = expand.grid(C = seq(0.1,100,length.out = 8), sigma=seq(0.01,50,length.out = 4)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "ROC")
svm.fit #information about the resampling
ggplot(svm.fit) #plot the summary metric as a function of the tuning parameter
str(svm.fit$resample) #resampling metrics


#--------------------------------------------------------------------------------
#------------------------   2do punto    ----------------------------------
#--------------------------------------------------------------------------------



## Load and processing of the filefile -------------------------------------------------------------------------------------
fdata = read.table("PBR.dat", sep = "", header = TRUE, stringsAsFactors = TRUE)
str(fdata)
fdata <- fdata[c(2:7,9)]
colnames(fdata)[7] <- "Y"
fdata <- fdata[c(1,4,7)]

fdata <- na.omit(fdata)


#---------------------------------      Explore the data       ----------------------------------


ggplot(data = fdata) + geom_point(mapping = aes(x = displ, y = hwy, size = cyl, color = class))
ggplot(fdata)+geom_point(aes(x=X1,y=X3,color=Y))
ggplot(fdata)+geom_point(aes(x=X1,y=X2,color=Y))
ggpairs(fdata,aes( alpha = 0.3))


#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "circle")




## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Salary,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fdata_train = fdata[trainIndex,]
fdata_val = fdata[-trainIndex,]

#Create index to select input variables
varindex <- variable.names(fdata) != "Y"



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



## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Y,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fdata_train = fdata[trainIndex,]
fdata_val = fdata[-trainIndex,]

#Create index to select input variables
varindex <- variable.names(fdata) != "Y"



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


#Evaluate the model with training sets and diagnosis
fdataTR_eval = fdata_train
fdataTR_eval$lm_pred = predict(lm.fit,  newdata = fdata_train)  

source("plotModelDiagnosis.R")
plotModelDiagnosis(fdata_train[,varindex], fdata_train$Salary, fdataTR_eval$lm_pred)

#Relation between residual and Years
ggplot(fdataTR_eval)+geom_point(aes(x=Years, y = Salary-lm_pred))

#-------------------------------------------------------------------------------------------------
#-------------------------------- Polynomial regression ------------------------------------------
#-------------------------------------------------------------------------------------------------
#Create new variables
fdata_train_poly <- fdata_train
fdata_train_poly$Years2 <- fdata_train_poly$Years^2
set.seed(150) #For replication
poly.fit = train(form = Salary~.,
                 data = fdata_train_poly, 
                 method = "lm", #Linear model
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
poly.fit #information about the resampling settings
summary(poly.fit)  #information about the model trained

#Evaluate the polynomial model with training sets and diagnosis
fdata_train_poly$lm_pred = predict(poly.fit,  newdata = fdata_train_poly)  

ggplot(fdata_train_poly)+geom_point(aes(x=Years, y = Y-lm_pred))


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
Smooth.fit = train(form = Salary~Years,
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
gam.fit = train(form = Y~X1,
                data = fdata_train, 
                method = "gamSpline",
                tuneGrid = data.frame(df = 6),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
gam::plot.gam(gam.fit$finalModel, se=TRUE ,col ="blue ")











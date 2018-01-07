#Ideas de la practca de clasificacion de Machine Learning

rm(list=ls())
library(dplyr)
library(ggplot2)
library(GGally)
library(readr)
library(caret)
library(pROC)


setwd('/Users/borisca/Desktop/MBD.comillas/ML1/practicas' )

Diabetes = read.table("~/Desktop/MBD.comillas/ML1/practicas/Diabetes.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
str(Diabetes)
summary(Diabetes)

vars=c('PREGNANT','GLUCOSE','BLOODPRESS','SKINTHICKNESS','INSULIN','BODYMASSINDEX','PEDIGREEFUNC','AGE')
Diabetes$DIABETES <- as.factor(Diabetes$DIABETES)
levels(Diabetes$DIABETES)=c('NO','YES'')

# Poner como NAs estos datos
for (j in 2:6) {
  for (i in 1:768) {
    if (Diabetes[i,j]==0) Diabetes[i,j]<-NA
  }
}
rm(i,j)

Diabetes %>%
  filter(GLUCOSE==0) %>%
  count(.)
Diabetes %>%
  filter(BLOODPRESS==0) %>%
  count(.)
Diabetes %>%
  filter(SKINTHICKNESS==0) %>%
  count(.)
Diabetes %>%
  filter(INSULIN==0) %>%
  count(.)
Diabetes %>%
  filter(BODYMASSINDEX==0) %>%
  count(.)
Diabetes %>%
  filter(DIABETES!=0) %>%
  count(.)

# LabAssignment_1
# Patxi


#---------------------------------------------------------------------------------------------------------
# FUNCTIONS
function.create_jpeg_filename <- function(name) {
  # add '.jpeg' to the filename
  paste(name,'.jpeg' , sep = '')
}

function.create_filename <- function(plot_type , name1 , name2) {
  # create a filename for plots
  # plot_type , name1 , name2
  filename = paste(plot_type , name1 , name2 , sep = '_')
  function.create_jpeg_filename(filename)
}

function.create_sorted_plots <- function(df) {
  # create sorted plots of all variables in a df and
  # saves them into a jpeg file in the working directory
  for (i in 1:length(df)) {
    filename = function.create_filename(plot_type = 'sort' , name1 = deparse(substitute(df)) , name2 = names(df)[i])
    jpeg(filename = function.create_filename(plot_type = 'sort' , name1 = deparse(substitute(df)) , name2 = names(df)[i]))
    plot(sort(Diabetes[,i]))
    dev.off()
  }
}

function.how_many_zeros <- function(df) {
  # Returns a dataframe with the variable name
  # how many zeros it contains
  y <- c()
  for (chr in names(df)) {
    x <- df %>% filter(get(chr)==0) %>% select(chr) %>% count(.)
    y <- c(y , as.integer(x))
  }
  y[is.na(y)] <- 0
  df <- data.frame(Variable=names(df),
                   count_zeros=y,
                   stringsAsFactors=FALSE)
  return(df)
}

#---------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# Data exploration

summary(Diabetes)
anyNA(Diabetes) # No hay ningun NA

#---------------------------------------------------------------------------------------------------------
# Analisis de Datos Que creo que han sido mal recogidos

function.how_many_zeros(Diabetes)
function.create_sorted_plots(df = Diabetes)





#---------------------------------------------------------------------------------------------------------
ggpairs(Diabetes)

#---------------------------------------------------------------------------------------------------------
# EDAD
boxplot(Diabetes$AGE)
# Sesgp hacia mujeres jovenes
t.test(Diabetes$AGE~Diabetes$DIABETES)

# PARA PODER VER COMO AFECTA LA EDAD EN LA DIABETES
p1 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$AGE , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")
p1
# f_DIABETES(EDAD) 

#---------------------------------------------------------------------------------------------------------
# EMBARAZO
t.test(Diabetes$PREGNANT~Diabetes$DIABETES)
boxplot(Diabetes$PREGNANT)
p2 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$PREGNANT , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")
# f_DIABETES(EMBARAZO)

#---------------------------------------------------------------------------------------------------------
# BLOODPRESS
t.test(Diabetes$BLOODPRESS~Diabetes$DIABETES)
boxplot(Diabetes$BLOODPRESS)
p3 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$BLOODPRESS , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")


#---------------------------------------------------------------------------------------------------------
# Insulin


p4 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$INSULIN , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")

#---------------------------------------------------------------------------------------------------------
# GLUcose

t.test(Diabetes$GLUCOSE~Diabetes$DIABETES)
p5 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$GLUCOSE , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")

#---------------------------------------------------------------------------------------------------------
# SKINTHICKNESS

t.test(Diabetes$SKINTHICKNESS~Diabetes$DIABETES)
p6 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$SKINTHICKNESS , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")

#---------------------------------------------------------------------------------------------------------
# BODYMASSINDEX

t.test(Diabetes$BODYMASSINDEX~Diabetes$DIABETES)
p7 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$BODYMASSINDEX , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")

#---------------------------------------------------------------------------------------------------------
# GLUcose

t.test(Diabetes$PEDIGREEFUNC~Diabetes$DIABETES)
p8 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$PEDIGREEFUNC , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 ) + theme(legend.position="None")

p9 <- ggplot(Diabetes) +
  geom_density( aes(Diabetes$GLUCOSE , 
                    fill=Diabetes$DIABETES) , 
                alpha=0.5 )
p9




multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=3)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##############################################################################################
#################################    glm Classification      #################################
##############################################################################################

set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(Diabetes$DIABETES,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
Diabetes_train = Diabetes[trainIndex,]
Diabetes_val = Diabetes[-trainIndex,] #este negativo corresponde a la negacion del conjunto, su compleento



## Initialize trainControl -----------------------------------------------------------------------
ctrl <- trainControl(method = "none",                      # method= "none" when no resampling is used
                     summaryFunction = twoClassSummary,    #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE,                    #Compute class probs in Hold-out samples
                     returnResamp = "all",                 #Return all information about resampling
                     savePredictions = TRUE)               #save predictions
  
# 'GLUCOSE','BODYMASSINDEX', 'AGE'
Diabetes_train[,c(5,6,8)]

## Train model -----------------------------------------------------------------------------------
set.seed(150) #For replication
#Train model using training data
LogReg.fit <- train(Diabetes_train[,c(5,6,8)], #Input variables. 
                   y=Diabetes_train$DIABETES,
                   method = "glm",             #Train linear regression
                   preProcess = c("center","scale"), #Center an scale
                   trControl = ctrl,           #trainControl Object
                   metric = "ROC")             #summary metric for summarize resampling
LogReg.fit          #information about the resampling
summary(LogReg.fit) #detailed information about the fit of the final model
str(LogReg.fit) 

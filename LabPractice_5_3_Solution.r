#################################################################################
##############  LabPractice 5.3 Principal components ############################
##############     ----------- solution ---------    ############################
#################################################################################


## Load dataset -------------------------------------------------------------------------------------------------------
Countries <- read.table("Countries.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)

#Perform principal component analysis
Countries.pca<-prcomp(Countries[,2:9], center = TRUE, scale. = TRUE) 
summary(Countries.pca)

#Plot eigenvalues obtained
plot(Countries.pca,type="b")

#Calculate and plot variance explained
std_dev <- Countries.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(Countries.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(Countries.pca$rotation[,1],ylab="PC1")
barplot(Countries.pca$rotation[,2],ylab="PC2")
barplot(Countries.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
biplot(Countries.pca, scale = 0, xlabs = Countries$Country, cex = 0.8)

#################################################################################
##############        Principal components           ############################
##############     ----------- suckers ---------     ############################
#################################################################################




## Load dataset -------------------------------------------------------------------------------------------------------
suckers <- read.table("PlayerDataSP.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)

#Perform principal component analysis
suckers.pca<-prcomp(suckers[,2:38], center = TRUE, scale. = TRUE) 
summary(suckers.pca)
str(suckers.pca)


#Plot eigenvalues obtained
plot(suckers.pca,type="b")

#Calculate and plot variance explained
std_dev <- suckers.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(suckers.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(suckers.pca$rotation[,1],ylab="PC1")
barplot(suckers.pca$rotation[,2],ylab="PC2")
barplot(suckers.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
biplot(suckers.pca, scale = 0, xlabs = suckers$NAME, cex = 0.8)

colnames(suckers)[21]

#################################################################################
##############        Principal components           ############################
##############     ----------- wine ---------     ############################
#################################################################################




## Load dataset -------------------------------------------------------------------------------------------------------
wine <- read.table("wine_data.dat",header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Perform principal component analysis
wine.pca<-prcomp(wine[,1:14], center = TRUE, scale. = TRUE) 
summary(wine.pca)
str(wine.pca)


#Plot eigenvalues obtained
plot(wine.pca,type="b")

#Calculate and plot variance explained
std_dev <- wine.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(wine.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(wine.pca$rotation[,1],ylab="PC1")
barplot(wine.pca$rotation[,2],ylab="PC2")
barplot(wine.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
biplot(wine.pca, scale = 0, cex = 0.8)

colnames(wine)[21]


#################################################################################
##############        Principal components           ############################
##############     ----------- traffic ---------     ############################
#################################################################################




## Load dataset -------------------------------------------------------------------------------------------------------
traffic <- read.table("TrafficData.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)

#Perform principal component analysis
traffic.pca<-prcomp(traffic[,2:38], center = TRUE, scale. = TRUE) 
summary(traffic.pca)
str(traffic.pca)


#Plot eigenvalues obtained
plot(traffic.pca,type="b")

#Calculate and plot variance explained
std_dev <- traffic.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(Countri.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(traffic.pca$rotation[,1],ylab="PC1")
barplot(traffic.pca$rotation[,2],ylab="PC2")
barplot(traffic.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
biplot(traffic.pca, scale = 0, xlabs = traffic$NAME, cex = 0.8)

colnames(traffic)[21]

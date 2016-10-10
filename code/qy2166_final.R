##########################################################################
# Qing Yin (qy2166)                                                      #
# STAT W4415 Final                                                       #
##########################################################################
setwd("G:/Columbia/STAT W4415/final")
getwd()
##########################################################################
# Q1                                                                     #
# EXPLORE and DESCRIBE the dependency pattern across different variables #
##########################################################################
load("evaluation.RData")
##########################################################################
# BASIC EXPLORATORY DATA ANALYSIS                                        #
##########################################################################
# first we need to check the dimension of the data set
dim(score)
# then we summarize the data set
summary(score)
# we can find that the evaluation data are included in the column 6 to 33
# so we compress our data set
evaluation<-score[,6:33]
# we use histogram and box plot to check the raw distribution of our compressed data set
par(mfrow=c(2,3))
for(i in 1:28){
  hist(evaluation[,i],main="histogram of evaluation",breaks=10,col="dodgerblue2")
}
par(mfrow=c(2,3))
for(i in 1:28){
  boxplot(evaluation[,i],main="box plot of evaluation",col="forestgreen")
}
# we can check the outliers and normality using qq plot
par(mfrow=c(1,1))
MD<-function(X){
  mu<-apply(X,2,mean)
  sigma<-cov(X)
  md<-apply(X,1,function(x) sqrt(t(x-mu)%*%solve(sigma)%*%(x-mu)))
  return(md)
}
qqplot(qchisq(ppoints(length(MD(evaluation))),df=28),(MD(evaluation))^2,
       main=expression("Q-Q plot for"~~{chi^2}[nu==28]),
       xlab="theoretical quantile",ylab="sample quantile")
abline(a=0,b=1,col="red")
##########################################################################
# CORRELATION MATRIX                                                     #
##########################################################################
# we now compute the correlation matrix to get the pairwise correlation
# then we need to visualize the correlation matrix
eval.cor<-cor(evaluation)
# use correlation plot to visualize
par(mfrow=c(1,1))
library(corrplot)
corrplot(eval.cor,method="ellipse")
col.spe<-colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white","cyan","#007FFF","blue","#00007F"))
corrplot.mixed(eval.cor,upper="circle",lower="color",col=col.spe(100))
# use level plot to visualize
library(lattice)
levelplot(eval.cor)
##########################################################################
# PCA                                                                    #
##########################################################################
# use pca to find independent orthogonal principal components
eval.pca<-princomp(evaluation,cor=T)
summary(eval.pca)
plot(eval.pca,type="b",col="mediumorchid1",main="proportion of variance for pca")
plot(eval.pca,type="l",col="mediumorchid1",main="proportion of variance for pca")
biplot(eval.pca)
eval.pca$loadings
# we can notice that we need two principal components
# we can notice from biplot that on the first 2 principal components variables are correlated with each other
# we can notice from loadings that the 1st and 2nd principal components do not have strong correlations with variables
# we actually annot get enough useful information from pca
##########################################################################
# FA                                                                     #
##########################################################################
# use fa to find latent variables
# orthogonal rotation
# factors are independent
eval.fac.var1<-factanal(covmat=eval.cor,n.obs=5820,rotation="varimax",factors=2)
eval.fac.var2<-factanal(covmat=eval.cor,n.obs=5820,rotation="varimax",factors=3)
eval.fac.var1
eval.fac.var2
plot(eval.fac.var1$loadings,type="n",main="factor loadings (varimax)")
text(eval.fac.var1$loadings,labels=rownames(eval.cor),col="springgreen4")
# although the p-value is really small, we still use 2 factors
# oblique rotation
# factors can be correlated
eval.fac.pro1<-factanal(covmat=eval.cor,n.obs=5820,rotation="promax",factors=2)
eval.fac.pro2<-factanal(covmat=eval.cor,n.obs=5820,rotation="promax",factors=3)
eval.fac.pro1
eval.fac.pro2
plot(eval.fac.pro1$loadings,type="n",main="factor loadings (promax)")
text(eval.fac.pro1$loadings,labels=rownames(eval.cor),col="springgreen4")
# although the p-value is really small, we still use 2 factors
# factor 1: Q13 - Q28 questions related to instructors
# factor 2: Q1 - Q12 questions related to courses
##########################################################################
# CCA                                                                    #
##########################################################################
# we apply canonical correlation analysis to two sets to check the correlation between two sets
# first set is the evaluations related to courses and second set is the evaluations related to instructors
eval.x<-evaluation[,1:12]
eval.y<-evaluation[,-(1:12)]
# we do cca here
eval.can<-cancor(eval.x,eval.y)
eval.can
# check how many pairs of covariates we need to use to explain
barplot(eval.can$cor,col="sienna2",main="correlation between canonical covariates")
plot(as.matrix(eval.x)%*%eval.can$xcoef[,1],as.matrix(eval.y)%*%eval.can$ycoef[,1],xlab="F1",ylab="G1",main="correlation between F1 and G1",pch=20,col="maroon4")
plot(as.matrix(eval.x)%*%eval.can$xcoef[,2],as.matrix(eval.y)%*%eval.can$ycoef[,2],xlab="F2",ylab="G2",main="correlation between F2 and G2",pch=20,col="maroon4")
# we can find that the first two pairs of covariates are significantly correlated
# we can find F1 mainly represents Q10 G1 mainly represents Q16
# F2 mainly represents Q11 G2 mainly represents Q17
# after testing first 8 covariates are significant
library(CCP)
p.asym(rho=eval.can$cor,N=dim(eval.x)[1],p=dim(eval.x)[2],q=dim(eval.y)[2],tstat="Wilks")
###################################################################################
# Q2                                                                              #
# CLUSTER each video frame (based on the extracted features) into different groups#
###################################################################################
load("Gesture.RData")
###################################################################################
# DATA PREPROCESSING                                                              #
###################################################################################
# sort all data set by row numbers
a1_raw_sort<-a1_raw[order(as.numeric(rownames(a1_raw))),]
a1_va3_sort<-a1_va3[order(as.numeric(rownames(a1_va3))),]
a2_raw_sort<-a2_raw[order(as.numeric(rownames(a2_raw))),]
a2_va3_sort<-a2_va3[order(as.numeric(rownames(a2_va3))),]
a3_raw_sort<-a3_raw[order(as.numeric(rownames(a3_raw))),]
a3_va3_sort<-a3_va3[order(as.numeric(rownames(a3_va3))),]
b1_raw_sort<-b1_raw[order(as.numeric(rownames(b1_raw))),]
b1_va3_sort<-b1_va3[order(as.numeric(rownames(b1_va3))),]
b3_raw_sort<-b3_raw[order(as.numeric(rownames(b3_raw))),]
b3_va3_sort<-b3_va3[order(as.numeric(rownames(b3_va3))),]
c1_raw_sort<-c1_raw[order(as.numeric(rownames(c1_raw))),]
c1_va3_sort<-c1_va3[order(as.numeric(rownames(c1_va3))),]
c3_raw_sort<-c3_raw[order(as.numeric(rownames(c3_raw))),]
c3_va3_sort<-c3_va3[order(as.numeric(rownames(c3_va3))),]
# combine data into one matrix
# eliminate the variable timestamp
# relabel the data
a1<-cbind(a1_raw_sort,a1_va3_sort)
a2<-cbind(a2_raw_sort,a2_va3_sort)
a3<-cbind(a3_raw_sort,a3_va3_sort)
b1<-cbind(b1_raw_sort,b1_va3_sort)
b3<-cbind(b3_raw_sort,b3_va3_sort)
c1<-cbind(c1_raw_sort,c1_va3_sort)
c3<-cbind(c3_raw_sort,c3_va3_sort)
ges.time<-rbind(a1,a2,a3,b1,b3,c1,c3)
ges<-ges.time[,-19]
ges.names<-vector()
for(i in 1:1743){
  ges.names[i]<-paste("a1_",i,sep="")
}
for(i in 1744:3003){
  ges.names[i]<-paste("a2_",(i-1743),sep="")
}
for(i in 3004:4833){
  ges.names[i]<-paste("a3_",(i-3003),sep="")
}
for(i in 4834:5902){
  ges.names[i]<-paste("b1_",(i-4833),sep="")
}
for(i in 5903:7322){
  ges.names[i]<-paste("b3_",(i-5902),sep="")
}
for(i in 7323:8429){
  ges.names[i]<-paste("c1_",(i-7322),sep="")
}
for(i in 8430:9873){
  ges.names[i]<-paste("c3_",(i-8429),sep="")
}
rownames(ges)<-ges.names
###################################################################################
# BASIC EXPLORATORY DATA ANALYSIS                                                 #
###################################################################################
# we first check the dimension of the data set
dim(ges)
# we then summarize the data set
# and get the column variances
summary(ges)
apply(ges,2,var)
# we find that we need to standardize the data
# three ways of standardizing
# method 1
rge<-sapply(ges,function(x) diff(range(x)))
ges.sta1<-sweep(ges,2,rge,FUN="/")
apply(ges.sta1,2,var)
# method 2
ges.sd<-apply(ges,2,sd)
ges.sta2<-sweep(ges,2,ges.sd,FUN="/") 
apply(ges.sta2,2,var)
# method 3
ges.sta3<-scale(ges)
apply(ges.sta3,2,var)
# now we will get the correlation matrices for nonnormalized data set and normalized data sets and visualize them
ges.cor.non<-cor(ges)
ges.cor.nor1<-cor(ges.sta1)
ges.cor.nor2<-cor(ges.sta2)
ges.cor.nor3<-cor(ges.sta3)
par(mfrow=c(2,2))
library(corrplot)
corrplot(ges.cor.non,method="ellipse")
corrplot(ges.cor.nor1,method="ellipse")
corrplot(ges.cor.nor2,method="ellipse")
corrplot(ges.cor.nor3,method="ellipse")
# coordinate-oriented positive correlation
# we check the outliers here
par(mfrow=c(1,1))
library(mvoutlier)
aq.plot(ges)
# it seems that we get 2 obvious outliers and many obscure outliers 
# we ignore the outlier checking process and start to do clustering
###################################################################################
# K-MEANS CLUSTERING                                                               #
###################################################################################
set.seed(123)
par(mfrow=c(1,1))
# plot within-groups sum of squares against number of clusters.
wss<-(nrow(ges.sta2)-1)*sum(apply(ges.sta2,2,var))
for(i in 2:10){
  wss[i]<-sum(kmeans(ges.sta2,centers=i)$withinss)
}
plot(1:10,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares",col="slateblue4",main="within groups sum of aquares against number of clusters")
# try k = 3 (3 clusters)
ges.k3<-kmeans(ges.sta2,centers=3)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.k3<-rep("palegreen4",times=nrow(ges))
my.color.vector.k3[ges.k3$cluster==2]<-"royalblue3"
my.color.vector.k3[ges.k3$cluster==3]<-"indianred2"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="3-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k3)
###################################################################################
# 3d graph is not necessary
###################################################################################
library(scatterplot3d)
ges.k3.3d<-scatterplot3d(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3],xlab="PC 1",ylab="PC 2",zlab="PC 3",type="n",lwd=2,main="3-mean on pc plot")
ges.k3.3d.co<-ges.k3.3d$xyz.convert(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3])
text(ges.k3.3d.co$x,ges.k3.3d.co$y,ges.k3.3d.co$z,labels=rownames(ges),cex=0.7,col=my.color.vector.k3)
###################################################################################
# try k = 4 (4 clusters)
ges.k4<-kmeans(ges.sta2,centers=4)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.k4<-rep("palegreen4",times=nrow(ges))
my.color.vector.k4[ges.k4$cluster==2]<-"royalblue3"
my.color.vector.k4[ges.k4$cluster==3]<-"indianred2"
my.color.vector.k4[ges.k4$cluster==4]<-"khaki3"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="4-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k4)
plot(ges.pc$scores[,1],ges.pc$scores[,3],xlab="PC 1",ylab="PC 3",type="n",lwd=2,main="4-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,3],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k4)
###################################################################################
# 3d graph is not necessary
###################################################################################
library(scatterplot3d)
ges.k4.3d<-scatterplot3d(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3],xlab="PC 1",ylab="PC 2",zlab="PC 3",type="n",lwd=2,main="4-mean on pc plot")
ges.k4.3d.co<-ges.k4.3d$xyz.convert(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3])
text(ges.k4.3d.co$x,ges.k4.3d.co$y,ges.k4.3d.co$z,labels=rownames(ges),cex=0.7,col=my.color.vector.k4)
###################################################################################
# try k = 5 (5 clusters)
ges.k5<-kmeans(ges.sta2,centers=5)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.k5<-rep("palegreen4",times=nrow(ges))
my.color.vector.k5[ges.k5$cluster==2]<-"royalblue3"
my.color.vector.k5[ges.k5$cluster==3]<-"indianred2"
my.color.vector.k5[ges.k5$cluster==4]<-"khaki3"
my.color.vector.k5[ges.k5$cluster==5]<-"darkorchid3"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="5-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k5)
plot(ges.pc$scores[,1],ges.pc$scores[,3],xlab="PC 1",ylab="PC 3",type="n",lwd=2,main="5-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,3],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k5)
plot(ges.pc$scores[,1],ges.pc$scores[,5],xlab="PC 1",ylab="PC 5",type="n",lwd=2,main="5-mean on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,5],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.k5)
###################################################################################
# 3d graph is not necessary
###################################################################################
library(scatterplot3d)
ges.k5.3d<-scatterplot3d(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3],xlab="PC 1",ylab="PC 2",zlab="PC 3",type="n",lwd=2,main="5-mean on pc plot")
ges.k5.3d.co<-ges.k5.3d$xyz.convert(x=ges.pc$scores[,1],y=ges.pc$scores[,2],z=ges.pc$scores[,3])
text(ges.k5.3d.co$x,ges.k5.3d.co$y,ges.k5.3d.co$z,labels=rownames(ges),cex=0.7,col=my.color.vector.k5)
###################################################################################
###################################################################################
# MODEL-BASED CLUSTERING                                                          #
###################################################################################
library(mclust)
ges.mc<-Mclust(ges.sta2)
summary(ges.mc)
# 9 clusters with "VVV": ellipsoidal, varying volume, shape, and orientation
plot(ges.mc,data=ges.sta2,what="BIC")
# "EII": spherical, equal volume 
# "VII": spherical, unequal volume 
# "EEI": diagonal, equal volume and shape
# "VEI": diagonal, varying volume, equal shape
# "EVI": diagonal, equal volume, varying shape 
# "VVI": diagonal, varying volume and shape 
# "EEE": ellipsoidal, equal volume, shape, and orientation 
# "EEV": ellipsoidal, equal volume and equal shape
# "VEV": ellipsoidal, equal shape 
# "VVV": ellipsoidal, varying volume, shape, and orientation
# each frame with cluster name
ges.mc.v9<-ges.mc$classification
# frame names in each cluster
ges.mc.v9.n<-lapply(1:9,function(x) rownames(ges)[ges.mc.v9==x])  
# probabilities of belonging to each cluster for each frame :
ges.mc.v9.p<-round(ges.mc$z,2)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.m9<-rep("cornflowerblue",times=nrow(ges))
my.color.vector.m9[ges.mc$classification==2]<-"darkolivegreen4"
my.color.vector.m9[ges.mc$classification==3]<-"firebrick1"
my.color.vector.m9[ges.mc$classification==4]<-"goldenrod3"
my.color.vector.m9[ges.mc$classification==5]<-"darkturquoise"
my.color.vector.m9[ges.mc$classification==6]<-"lightgreen"
my.color.vector.m9[ges.mc$classification==7]<-"hotpink1"
my.color.vector.m9[ges.mc$classification==8]<-"orange1"
my.color.vector.m9[ges.mc$classification==9]<-"purple4"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="model-based 9-cluster on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.m9)
summary(ges.pc)
# we can find that we need first 15 pc's actually
###################################################################################
# HIERARCHICAL CLUSTERING                                                         #
###################################################################################
# Calculating pairwise Euclidean distances between the (standardized) frames:
ges.dist<-dist(ges.sta2)
# start with complete linkage
ges.cl<-hclust(ges.dist,method="complete")
# plot the complete-linkage dendrogram
plot(ges.cl,labels=rownames(ges),ylab="Distance")
# cut the complete-linkage dendrogram to form 9 clusters here:
cut.9c<-cutree(ges.cl,k=9)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.h9c<-rep("cornflowerblue",times=nrow(ges))
my.color.vector.h9c[cut.9c==2]<-"darkolivegreen4"
my.color.vector.h9c[cut.9c==3]<-"firebrick1"
my.color.vector.h9c[cut.9c==4]<-"goldenrod3"
my.color.vector.h9c[cut.9c==5]<-"darkturquoise"
my.color.vector.h9c[cut.9c==6]<-"lightgreen"
my.color.vector.h9c[cut.9c==7]<-"hotpink1"
my.color.vector.h9c[cut.9c==8]<-"orange1"
my.color.vector.h9c[cut.9c==9]<-"purple4"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="hierarchical complete-linkage 9-cluster on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.h9c)
# then average linkage
ges.al<-hclust(ges.dist,method="average")
# plot the average-linkage dendrogram
plot(ges.al,labels=rownames(ges),ylab="Distance")
# cut the average-linkage dendrogram to form 9 clusters here:
cut.9a<-cutree(ges.al,k=9)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.h9a<-rep("cornflowerblue",times=nrow(ges))
my.color.vector.h9a[cut.9a==2]<-"darkolivegreen4"
my.color.vector.h9a[cut.9a==3]<-"firebrick1"
my.color.vector.h9a[cut.9a==4]<-"goldenrod3"
my.color.vector.h9a[cut.9a==5]<-"darkturquoise"
my.color.vector.h9a[cut.9a==6]<-"lightgreen"
my.color.vector.h9a[cut.9a==7]<-"hotpink1"
my.color.vector.h9a[cut.9a==7]<-"orange1"
my.color.vector.h9a[cut.9a==7]<-"purple4"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="hierarchical average-linkage 9-cluster on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.h9a)
# then single linkage
ges.sl<-hclust(ges.dist,method="single")
# plot the single-linkage dendrogram
plot(ges.sl,labels=rownames(ges),ylab="Distance")
# cut the single-linkage dendrogram to form 9 clusters here:
cut.9s<-cutree(ges.sl,k=9)
# visualize clusters on first 2 pc's
ges.pc<-princomp(ges,cor=T)
my.color.vector.h9s<-rep("cornflowerblue",times=nrow(ges))
my.color.vector.h9s[cut.9s==2]<-"darkolivegreen4"
my.color.vector.h9s[cut.9s==3]<-"firebrick1"
my.color.vector.h9s[cut.9s==4]<-"goldenrod3"
my.color.vector.h9s[cut.9s==5]<-"darkturquoise"
my.color.vector.h9s[cut.9s==6]<-"lightgreen"
my.color.vector.h9s[cut.9s==7]<-"hotpink1"
my.color.vector.h9s[cut.9s==7]<-"orange1"
my.color.vector.h9s[cut.9s==7]<-"purple4"
plot(ges.pc$scores[,1],ges.pc$scores[,2],xlab="PC 1",ylab="PC 2",type="n",lwd=2,main="hierarchical single-linkage 9-cluster on pc plot")
text(ges.pc$scores[,1],ges.pc$scores[,2],labels=rownames(ges),cex=0.7,lwd=2,col=my.color.vector.h9s)

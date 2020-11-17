# K-MEANS-CLUSTER-IRIS-DATASET
setwd("F:/internship")
Irisdata<- read.csv("Iris.csv")
Irisdata <- Irisdata[-1]
summary(Irisdata)
str(Irisdata)
library(ggplot2)
attach(Irisdata)

sum(is.na(Irisdata))
#unique(), eliminates duplicate elements/rows from a vector, data frame or array.
Irisdata=unique(Irisdata)


library(DataExplorer)
plot_missing(Irisdata)
plot_str(Irisdata)
plot_histogram(Irisdata)
plot_density(Irisdata)
plot_correlation(Irisdata, type = 'continuous')

boxplot(SepalLengthCm,SepalWidthCm,PetalLengthCm,PetalWidthCm,
        main=" Multiple Boxplot Comparision",
names=c("SepalLengthCm","SepalWidthCm","PetalLengthCm","PetalWidthCm"),col=c("orange","red"))


IrisggSL<-ggplot(data=Irisdata,aes(x=Species,y= SepalLengthCm,colour=Species))
IrisggSL+geom_jitter()+geom_boxplot(size=1,alpha=0.5)+ggtitle("Species vs SepalLength")+
  theme(axis.title.x = element_text(colour="Blue",size=15),
        axis.title.y = element_text(colour="Red",size=15),
        axis.text.x = element_text(colour="Blue",size=10),
        axis.text.y = element_text(colour="Red",size=15),
        legend.title=element_text(colour="Red",size=15),
        legend.text=element_text(colour="DarkGreen",size=12),
        plot.title = element_text(colour="DarkBlue",size=16,family="Courier"))

IrisggSW<-ggplot(data=Irisdata,aes(x=Species,y= SepalWidthCm,colour=Species))
IrisggSW+geom_jitter()+geom_boxplot(size=1,alpha=0.5)+ggtitle("Species vs SepalWidth")+
  theme(axis.title.x = element_text(colour="Blue",size=15),
        axis.title.y = element_text(colour="Red",size=15),
        axis.text.x = element_text(colour="Blue",size=10),
        axis.text.y = element_text(colour="Red",size=15),
        legend.title=element_text(colour="Red",size=15),
        legend.text=element_text(colour="DarkGreen",size=12),
        plot.title = element_text(colour="DarkBlue",size=16,family="Courier"))


IrisggPL<-ggplot(data=Irisdata,aes(x=Species,y= PetalLengthCm,colour=Species))
IrisggPL+geom_jitter()+geom_boxplot(size=1,alpha=0.5)+ggtitle("Species vs PetalLength")+
  theme(axis.title.x = element_text(colour="Blue",size=15),
        axis.title.y = element_text(colour="Red",size=15),
        axis.text.x = element_text(colour="Blue",size=10),
        axis.text.y = element_text(colour="Red",size=15),
        legend.title=element_text(colour="Red",size=15),
        legend.text=element_text(colour="DarkGreen",size=12),
        plot.title = element_text(colour="DarkBlue",size=16,family="Courier"))

IrisggPW<-ggplot(data=Irisdata,aes(x=Species,y= PetalWidthCm,colour=Species))
IrisggPW+geom_jitter()+geom_boxplot(size=1,alpha=0.5)+ggtitle("Species vs PetalWidth")+
  theme(axis.title.x = element_text(colour="Blue",size=15),
        axis.title.y = element_text(colour="Red",size=15),
        axis.text.x = element_text(colour="Blue",size=10),
        axis.text.y = element_text(colour="Red",size=15),
        legend.title=element_text(colour="Red",size=15),
        legend.text=element_text(colour="DarkGreen",size=12),
        plot.title = element_text(colour="DarkBlue",size=16,family="Courier"))


IQR(SepalWidthCm)
OutSepalWidthCm<-summary(SepalWidthCm)[["3rd Qu."]]+1.5*0.5
OutSepalWidthCm
OutSepalWidthCm<-summary(SepalWidthCm)[["1st Qu."]]-1.5*0.5
OutSepalWidthCm

# We will use elbow method to find cluster
set.seed(123456) # to get the same result everytime
Irisdf<- Irisdata[1:4]
WCSS<-vector()      # we did an empty vector
for(i in 1:8)WCSS[i]<-sum(kmeans(Irisdf,i)$withinss)
plot(1:8,WCSS,type="b",main=paste("cluster of clients"),xlab="no of clients",ylab="WCSS")

kmeans<-kmeans(Irisdf,centers=3,iter.max=500,nstart=40)
#visualising the cluster
library(cluster)
clusplot(Irisdf,
         kmeans$cluster,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2, 
         plotchar=TRUE,
         span=TRUE,
         main=paste("cluster of clients"),xlab="Species",ylab="Lengths")


## profiling the clusters
Irisdf$cluster <- kmeans$cluster
Irisdf$cluster
aggr<-aggregate(Irisdf,list(Irisdf$cluster),mean)
clus.profile<-data.frame(Cluster=aggr[,1],
                         Freq=as.vector(table(Irisdf$cluster)),
                         aggr[,-1])
View(clus.profile)
print(clus.profile)

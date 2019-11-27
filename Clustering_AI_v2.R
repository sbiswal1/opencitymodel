library(package) # for data cleaning

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

#install.packages("foreign")
library(foreign)
library(fpc)




mydata <- read.csv("C:/Users/sushbiswal/Desktop/AI_Clustering/Clean_Transformed_Data_051218.csv")


mydata<-mydata[mydata$Q26_ALL!=0,]
mydata<-mydata[complete.cases(mydata),]
colnames(mydata)

mydata$Q15_1<-as.numeric(mydata$Q15_1)

mydata$Q25_ALL<-as.numeric(mydata$Q25_ALL)
unique(mydata$Q25_ALL)
unique(mydata$Q26_ALL)
unique(mydata$Q18_1)
unique(mydata$Q36)

unique(mydata$Q38_ALL)

unique(mydata$Q39_ALL)
library(dummies)


reclass<-function(x) {
  if(x ==8){
    "NA"
  }
  else {
    8-x
  }
}

unique(mydata$Q26_ALL)
mydata$Q26_ALL<-as.numeric(mydata$Q26_ALL)
mydata$Q18_1<-as.numeric(mydata$Q18_1)

mydata$Q36<-as.factor(mydata$Q36)

mydata$Q45_new<-as.factor(mydata$Q45_new)

mydata$Q37_new<-as.factor(mydata$Q37_new)



mydata$Q38_ALL<-as.numeric(mydata$Q38_ALL)

mydata$Q39_ALL<-as.numeric(mydata$Q39_ALL)

mydata$Q11_new<-as.factor(mydata$Q11_new)

####PAM gower
colnames(mydata[3:12])
gower_dist <- daisy(mydata[3:12],metric = "gower",type = list(logratio = 3), warnType = TRUE)



####
sil_width <- c(NA)
# 
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# 
# 
# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

### kmeans
Kfit4 <- kmeans(mydataClus[,-2], 4)
require(cluster)
clusplot(mydataClus,Kfit4,color = T)
###
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

pam_fit$silinfo
###

PAM<-pam(gower_dist, 3)
clusplot(mydata, PAM$clustering, color = T, main = 'Cluster Plot')

mydata$cluster<-PAM$clustering
write.csv(mydata,"C:/Users/sushbiswal/Desktop/AI_Clustering/PAM_results.csv")
unique(PAM$clustering)
###

kmax<-10
WSSus_arrests<-sapply(1:kmax, function(k) kmeans(mydata, centers = k, nstart = 10)$tot.withinss)

plot(1:kmax, WSSus_arrests, type = 'b', xlab = 'k', ylab = 'Total wss')
abline(v=4, lty=2)


###
d <- dist(mydata[,-2], method = "euclidean")

PAM<-pam(d, 4)
clusplot(mydata, PAM$clustering, color = T, main = 'Cluster Plot')
table(PAM$clustering)


PAM$silinfo
kmax<-10
WSSus_arrests<-sapply(1:kmax, function(k) pam(d, nstart = 10)$tot.withinss)

plot(1:kmax, WSSus_arrests, type = 'b', xlab = 'k', ylab = 'Total wss')
abline(v=4, lty=2)



###

str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring


str(si <- silhouette(PAM))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring


str(si <- silhouette(Kfit4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

clusplot(mydata, Kfit4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

fit<-kmeans(d,4,nstart=10)
plotcluster(mydata, fit$cluster)
print(fit$cluster)
table(fit$cluster)


mydata$cluster

####
colnames(mydata)
colnames(ndata)
ndata<- dummy.data.frame(mydata, sep = ".",names = c("Q36","Q45_new","Q37_new"))
colnames(ndata[3:22])
colnames(ndata[3:22])

ndata[3:17] <- lapply(ndata[3:17], as.numeric)

d <- dist(ndata[3:17], method = "euclidean")
View(ndata)
PAM<-pam(d, 3)
clusplot(ndata[3:], PAM$clustering, color = T, main = 'Cluster Plot')
table(PAM$clustering)
PAM$silinfo

kmax<-10
WSSus_arrests<-sapply(1:kmax, function(k) pam(d, nstart = 10)$tot.withinss)

plot(1:kmax, WSSus_arrests, type = 'b', xlab = 'k', ylab = 'Total wss')
abline(v=4, lty=2)

###
colnames(mydata)
mydata<-mydata[,-6]

mydata<-mydata[,-8]
library(clustMixType)
# apply k prototyps
kpres <- kproto(mydata[3:9], 4)

kpres$centers
kpres$withinss/kpres$tot.withinss
table(kpres$cluster)

### elbow
wss<-vector()
for (i in 2:15){ wss[i] <- sum(kproto(mydata[3:12], i)$withinss)}
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

clprofiles(kpres, mydata[3:12], vars = NULL, col = NULL)

###
str(si <- silhouette(kpres))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

clusplot(mydata[3:12], kpres$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
table(kpres$cluster)

tsne_obj <- Rtsne(kpres)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kpres$cluster),
         name = mydata$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


calinhara(m[3:12],kpres$cluster)


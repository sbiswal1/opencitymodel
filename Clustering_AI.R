package <- c('dplyr','ISLR','cluster','Rtsne','ggplot2')

install.packages(package)

library(package) # for data cleaning

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

#install.packages("foreign")
library(foreign)
#install.packages("clustMixType")
library(clustMixType)


mydata <- read.csv("C:/Users/sushbiswal/Desktop/AI_Clustering/Clean_Data_Transformed_final.csv")
str(mydata)
head(mydata,10)
# ###
unique(mydata$Q20)
table(mydata$Q40_new1)
###
mydata$Q40_new
colnames(mydata)
mydataClus <- subset(mydata, select=c("RawID","Q13","Q14_1","Q15_1","Q18_1","Q34_1","Q40_new","Q20_new","Q41","CurrentUsageofAI"))
View(mydataClus)

mydataClus[mydataClus$RawID =='463',]

mydataClus[mydataClus$RawID =='2870',]

mydataClus[mydataClus$RawID =='5388',]
### declaring types of variables
mydataClus<-mydataClus[complete.cases(mydataClus),]
mydataClus[is.na(mydataClus), ]

mydataClus[is.na(mydataClus), ]
unique(mydataClus$Q20_new)

mydataClus$Q40_new[is.na(mydataClus$Q40_new), ]
mydataClus$CurrentUsageofAI<-factor(mydataClus$CurrentUsageofAI)


mydataClus$Q13<-as.numeric(mydataClus$Q13)

mydataClus$Q14_1<-as.ordered(mydataClus$Q14_1)

mydataClus$Q15_1<-as.ordered(mydataClus$Q15_1)

mydataClus$Q18_1<-as.numeric(mydataClus$Q18_1)

mydataClus$Q34_1<-as.ordered(mydataClus$Q34_1)

mydataClus$Q34_1<-as.ordered(mydataClus$Q34_1)


mydataClus$Q40_new<-factor(mydataClus$Q40_new)

mydataClus$Q20_new<-factor(mydataClus$Q20_new)

#mydataClus$CurrentUsageofAI<-as.ordered(mydataClus$CurrentUsageofAI)

mydataClus$Q41<-as.ordered(mydataClus$Q41)
#mydataClus$Q20_new<-as.ordered(mydataClus$Q20_new)
unique(mydataClus$Q40_new)

unique(mydata$Q13)
unique(as.ordered(mydataClus$Q20))

head(mydataClus$Q20_new,10)

head(mydataClus$Q40_new,10)
head(mydataClus$Q40_new,10)
head(mydataClus,10)

####

Regroup <- function(x) {
  if(x =='4 - High  (done often; a specialty of the company.)'){
    4
    }
  else{0}
}


mydata$Q20_new<-apply(mydata[25], 1,Regroup1)
mydata$Q40_new1<-apply(mydata[184], 1,Regroup)
head(mydata[184],10)
head(mydata[25],10)
unique(mydataClus$Q40_new)
###
Regroup1 <- function(x) {
  if(x =='There are no strategies - just ad hoc AI/cognitive adoption at the department level or below'){
    1
  }else if(x =='Departments have set their own strategies for adopting AI/cognitive, but there are no company-wide guidelines'){
    2
  } else if(x =='Departments have set their own strategies for adopting AI/cognitive, based upon company-wide guidelines'){
    3
  } else if(x =='We have a comprehensive, detailed, company-wide strategy in place for AI/cognitive adoption that departments are expecte'){
    4
  } 
  else{"NA"}
}
###
# Ward Hierarchical Clustering
d <- dist(mydataClus[,-1], method = "euclidean") # find distance matrix

d <- dist(mydataClus, method = "manhattan")
fit <- hclust(d, method="ward.D") # apply hirarchical clustering 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into k no. of clusters
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the k no. of clusters 

###
head(mydataClus,10)
length(mydataClus[is.na(mydataClus)])



###
# K-Means Cluster Analysis

Kfit2 <- kmeans(mydataClus, 2) # 2 cluster solution
Kfit3 <- kmeans(mydataClus, 3) # 2 cluster solution
Kfit4 <- kmeans(mydataClus, 4) # 2 cluster solution
Kfit5 <- kmeans(mydataClus, 5) # 2 cluster solution


require(cluster)
clusplot(mydataClus,Kfit5,color = T)

# Calculate silhouette width for many k using PAM
head(d,10)
sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(d,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
  
  
###
  gower_dist <- daisy(mydataClus[, -1],metric = "gower",type = list(logratio = 3), warnType = TRUE)


# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)




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


###

pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


###
#Elbow Method for finding the optimal number of clusters

fit <- hclust(d, method="ward.D") # apply hirarchical clustering
fit <- hclust(gower_dist, method="complete")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into k no. of clusters
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the k no. of clusters 
plot(mydataClus, pch=20, col=cutree(fit,3))
tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(groups),
         name = mydataClus$RawID)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

mydataClus$cl_hierarchical = factor(cutree(fit, k=3)) 

head(factor(cutree(fit, 9)),10)
hc.norm = hclust(dist(tsne.norm$Y))
mydataClus$hclust = factor(cutree(fit, 9))
fit.cent = mydataClus %>% group_by(hclust) %>% select(tsne1, 
                                                         tsne2) %>% summarize_all(mean)


  # Elbow Method for finding the optimal number of clusters
  set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kproto(mydataClus, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

###dbscan
install.packages("fpc")
install.packages("dbscan")

library(fpc)
library(dbscan)


db <- fpc::dbscan(gower_dist, eps = 0.001, MinPts = 5)


plot(db, mydataClus, main = "DBSCAN", frame = FALSE)


tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(db$cluster))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) 


##
pam_fit <- pam(d, diss = TRUE, k = 4)

tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


###
library(cluster)
library(factoextra)
library(cluster)
tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(Kfit5$cluster),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


##
pam_fit <- pam(d, diss = TRUE, k = 4)

tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


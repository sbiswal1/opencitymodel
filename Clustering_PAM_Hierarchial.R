package <- c('dplyr','ISLR','cluster','Rtsne','ggplot2')

install.packages(package)



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
###
unique(mydata$Q20)
colnames(mydata)
mydataClus <- subset(mydata, select=c("RawID","Q13","Q14_1","Q15_1","Q18_1","Q34_1","Q40_new","Q20","Q41","CurrentUsageofAI"))


### declaring types of variables
mydataClus<-mydataClus[complete.cases(mydataClus),]
mydataClus[is.na(mydataClus), ]

mydataClus[is.na(mydataClus), ]
unique(mydata$Q15_1)

mydataClus$Q40_new[is.na(mydataClus$Q40_new), ]

mydataClus-cbind(mydataClus, dummy(mydataClus$CurrentUsageofAI, sep = "11_"))

mydataClus$Q13<-as.numeric(mydataClus$Q13)

mydataClus$Q14_1<-as.numeric(mydataClus$Q14_1)

mydataClus$Q15_1<-as.numeric(mydataClus$Q15_1)

mydataClus$Q18_1<-as.numeric(mydataClus$Q18_1)

mydataClus$Q34_1<-as.numeric(mydataClus$Q34_1)

library(dummies)

mydataClus <- cbind(mydataClus, dummy(mydataClus$Q40_new, sep = "Q40_"))

mydataClus <- cbind(mydataClus, dummy(mydataClus$Q20, sep = "Q20_"))

mydataClus<-cbind(mydataClus, dummy(mydataClus$CurrentUsageofAI, sep = "11_"))

mydataClus$Q40_new<-dummy (mydataClus$Q40_new)

mydataClus$Q20_new<-factor(mydataClus$Q20_new)

dummy()
colnames(mydataClus)

head(mydataClus,10)]
drops <- c("Q40_new","Q20","CurrentUsageofAI")
mydataClus<-mydataClus[ , !(names(mydataClus) %in% drops)]
#mydataClus$CurrentUsageofAI<-as.ordered(mydataClus$CurrentUsageofAI)

mydataClus$Q41<-as.numeric(mydataClus$Q41)


####distance

d <- dist(mydataClus[,-1], method = "euclidean")

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


### gowers distance +Pam

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





### PAM +eucleadian 


pam_fit <- pam(d, diss = TRUE, k = 3)

tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = mydataClus$RawID)
head(tsne_data,10)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

mydataClus$Cluster<-pam_fit$clustering

write.csv(mydataClus,"C:/Users/sushbiswal/Desktop/AI_Clustering/PAM_eucl_results3clust.csv")
### Heirarchial +eucledian 
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into k no. of clusters
rect.hclust(fit, k=4, border="red") # draw dendogram with red borders around the k no. of clusters 
plot(mydataClus, pch=20, col=cutree(fit,3))
tsne_obj <- Rtsne(d, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(groups),
         name = mydataClus$RawID)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


mydataClus$Cluster<-groups 

write.csv(mydataClus,"C:/Users/sushbiswal/Desktop/AI_Clustering/Hclust_eucl_results.csv")


###
####
sil_width <- c(NA)
# 
for(i in 2:10){
  
  pam_fit <- pam(d,
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

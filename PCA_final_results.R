#### reading the data from csv

f<-read.csv("C:/Users/sushbiswal/Desktop/PAM/PAM-2. Raw Data/Combined_Data/combineddata_2012_Feb21.csv") 

library(stats)
library(psych)

### removing the zeros
#g=f[f$PAMTTOT >0,]


g<-g[g$PRBBILS_R!='Missing',]
g<-g[g$PRBHHWK_R!='Missing',]
g<-g[g$PRBLHWK_R!='Missing',]


g<-g[g$PRBMEAL_R!='Missing',]


g<-g[g$PRBSHOP_R!='Missing',]
g<-g[g$PRBTELE_R!='Missing',]
g<-g[g$HPPDBATH_R!='Missing',]
g<-g[g$HPPDCHAR_R!='Missing',]
g<-g[g$HPPDDRES_R!='Missing',]
g<-g[g$HPPDEAT_R!='Missing',]
g<-g[g$HPPDTOIL_R!='Missing',]
g<-g[g$HPPDWALK_R!='Missing',]




### subset
ndata<-subset(f, select=c(
  "PRBBILS_R","PRBHHWK_R","PRBLHWK_R","PRBMEAL_R","PRBSHOP_R","PRBTELE_R","HPPDBATH_R","HPPDCHAR_R","HPPDDRES_R","HPPDEAT_R","HPPDTOIL_R","HPPDWALK_R"
))		

		


##### Combining the variables

ndata$PRBBILS_R <-ifelse(ndata$PRBBILS_R == "Yes",1,0)

ndata$PRBHHWK_R <-ifelse(ndata$PRBHHWK_R == "Yes", 1,0)
ndata$PRBLHWK_R <-ifelse(ndata$PRBLHWK_R == "Yes", 1,0)

ndata$PRBMEAL_R <-ifelse(ndata$PRBMEAL_R == "Yes", 1,0)
ndata$PRBSHOP_R <-ifelse(ndata$PRBSHOP_R == "Yes", 1,0)
ndata$PRBTELE_R <-ifelse(ndata$PRBTELE_R == "Yes", 1,0)


ndata$HPPDBATH_R <-ifelse(ndata$HPPDBATH_R == "Yes", 1,0)
ndata$HPPDCHAR_R <-ifelse(ndata$HPPDCHAR_R == "Yes", 1,0)
ndata$HPPDDRES_R<-ifelse(ndata$HPPDDRES_R == "Yes", 1,0)
ndata$HPPDEAT_R<-ifelse(ndata$HPPDEAT_R == "Yes", 1,0)
ndata$HPPDTOIL_R<- ifelse(ndata$HPPDTOIL_R == "Yes", 1,0)
ndata$HPPDWALK_R<- ifelse(ndata$HPPDWALK_R == "Yes", 1,0) 

### correlation matrix

library(psych)
cormatrix<-(tetrachoric(ndata))
cormatrix$rho

### KMO Test, Bartlett

KMO(as.matrix(cormatrix$rho))
cortest.bartlett(ndata)
str(ndata)
det(as.matrix(cormatrix$rho))

####unrotated PCA
prin<-princomp(covmat=as.matrix(cormatrix$rho))
summary(prin)


## rotated PCA
pc2<-principal(r=as.matrix(cormatrix$rho), nfactors=3, rotate = "varimax",scores = TRUE)
pc2<-principal(ndata, nfactors=3, rotate = "varimax",scores = TRUE)

pc2$communality
pc2$fit
pc2$residual
pc2$values
pc2$Structure
predict.psych(pc2,ndata,as.matrix(cormatrix$rho))
### binding scores to data
pc2$r.scores
new<-cbind(f, predict.psych(pc2,ndata,as.matrix(cormatrix$rho)))
unique(new$RC1)
pc2$scores
round(cov(g,pc2$scores),2) 
pc2$loadings
summary(ndata_new$RC1)
plot(pc2$values, type = 'b')
pc2$scores
write.csv(new,"C:/Users/sushbiswal/Desktop/PAM/PAM-2. Raw Data/Combined_Data/combineddata_2012_March21.csv")
### writing 
unique(new$RC1)
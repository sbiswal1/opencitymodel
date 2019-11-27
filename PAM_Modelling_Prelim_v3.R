library(dummies)
data<-read.csv("C:/Users/sushbiswal/Desktop/PAM/PAM-2. Raw Data/Combined_Data/combineddata_2012_Feb21.csv") 
f<-read.csv("C:/Users/sushbiswal/Desktop/PAM/PAM-2. Raw Data/Combined_Data/combineddata_2012_March21.csv")
class(f$CS1YRWGT_cost)<numeric()
ndata<-subset(data, select=c('PAMTTOT', "PAS","H_CENSUS","H_METRO","ROSTSEX","H_AGE",
                             "Age_bin","Income_R","JobSatus","Health_status","cancer",
                             "Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","CS1YRWGT_cost",
                             "D_MA","Dual_Eligibility","Diabetes","Lungcond","Depression",
                             "Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                            "Smoking","BMI_Class","RC1","RC2","RC3","H_PMTVST","H_INPSTY"))

unique(ndata$ROSTSEX)
print(paste(i))
print(colnames(ndata))
min(ndata$PAMTTOT)
g<-(ndata$PAMTTOT[ndata$PAMTTOT==0])
length(g)
### removing missing data
ndata<-ndata[ndata$PAMTTOT>0,]
ndata<-ndata[ndata$Spdegrcv_R!='Missing',]
ndata<-ndata[ndata$Spmarsta!="Missing",]

ndata<-ndata[ndata$Income_R!="Missing",]

ndata<-ndata[ndata$Race_origin!="Missing",]
ndata<-ndata[ndata$D_HHTOT._R!="FALSE",]

ndata<-ndata[ndata$Health_status!="missing",]
ndata<-ndata[ndata$JobSatus!="missing",]


ndata<-ndata[ndata$Age_bin!= 'lessthan65',]

ndata$ROSTSEX <-ifelse(ndata$ROSTSEX == 1, "Male", "Female")

#ndata$BMI_Class <-ifelse(ndata$ROSTSEX == 1, "Male", "Female")
ndata$D_MA1[ndata$D_MA==1]<-"Yes"
ndata$D_MA1[ndata$D_MA==2]<-"Yes"
ndata$D_MA1[ndata$D_MA==3]<-"Yes"
ndata$D_MA1[ndata$D_MA==0]<-"No"
ndata$D_MA1[ndata$D_MA=='NA']<-"No"

ndata$BMI_Class[ndata$BMI_Class==1]<-"Overweight"
ndata$BMI_Class[ndata$BMI_Class==2]<-"Obese"
ndata$BMI_Class[ndata$BMI_Class==0]<-"Normal"
levels(ndata$D_HHTOT._R) <- list(fourplus="four people",fourplus="Five plus",twothree="three people",twothree ="two people",oneperson="oneperson")



###
ndata$H_INPSTY_class<-ifelse(ndata$H_INPSTY<=0, "NoVisits", ifelse(ndata$H_INPSTY==1,"1","Above2"))
ndata$H_PMTVST_class<-ifelse(ndata$H_PMTVST<=0, "NoVisits", ifelse(ndata$H_PMTVST>0 & ndata$H_PMTVST<=4,"1--4",
                                                                   ifelse(ndata$H_PMTVST>4 & ndata$H_PMTVST<=9, "5--9", "Above10")))
hist((ndata$PAMTTOT))
hist(ndata$PAMTTOT)
boxcox(fit)
ndata_new<-ndata_new[!ndata_new$PAMTTOT==0.0000000000000000000001,]<-0.0000000000000000000001
ndata_new$PAMTTOT[ndata_new$PAMTTOT==0]
###
ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_INPSTY_class","H_PMTVST_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R","Smoking","BMI_Class","H_PMTVST_new","H_INPSTY_new"))
class(data$CS1YRWGT_cost)

ndata_new$CS1YRWGT_cost
ndata_new$`PAS.High Activation`
ndata_new$PAMTTOT
fit<-lm(log(ndata_new$PAMTTOT) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
        +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
        +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
        +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
        +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
        +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
        +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
        +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
        +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
        +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
        +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily
        +ndata_new$Dwelling_R.Apartment+ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
        +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
        +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+
        +ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
        +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6+ndata_new$H_CENSUS.7
        +ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9+ndata_new$H_CENSUS.10
        +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight+ndata_new$BMI_Class.Normal
        +ndata_new$`H_PMTVST_class.1--4`+ndata_new$`H_PMTVST_class.5--9`+ndata_new$H_PMTVST_class.Above10
        +ndata_new$H_PMTVST_class.NoVisits+ndata_new$H_INPSTY_class.1+ndata_new$H_INPSTY_class.Above2
        +ndata_new$H_INPSTY_class.NoVisits
        , weights = ndata_new$CS1YRWGT_cost,data = ndata_new)


summary(fit)



lmOut <- function(res, file="test.csv", ndigit=3, writecsv=T) {
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+2)
  
  G[1,1] <- toString(res$call)
  G[1,1] <- toString(res$call)
  
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncol+1)] <- colnames(co)
  
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)
  
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  #Save sample size
  G[1,6] <- "Sample size"
  G[2,6] <- as.character(nrow(fit$model))
  
  print(G)
  if (writecsv) write.csv(G, file=file, row.names=F)
}

lmOut(fit,paste("C:/PAM",name,sep="/"))

name=paste('PAM_adding_utilization_14082018',".csv",sep = "")
lmOut(fit,paste("C:/Users/sushbiswal/Desktop/PAM",name,sep="/"))

mean(ndata_new$PAMTTOT)
weighted.mean()

summary(fit)
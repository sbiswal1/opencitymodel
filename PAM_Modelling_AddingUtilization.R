library(dummies)
source('C:/Users/sushbiswal/Desktop/PAM/writing_lm_output.R')
file<-read.csv("C:/Users/sushbiswal/Desktop/PAM/PAM-2. Raw Data/Combined_Data/combineddata_2012_March21.csv")
file$CS1YRWGT_cost

ndata<-subset(file, select=c('AAMTTOT_OP','H_PMTCHO','H_PMTVST','H_INPSTY','H_INPDAY','H_OUTBIL' ,                                 
                             'AAMTTOT_PM','AAMTTOT_DU','AAMTTOT_FA','AAMTTOT_HH','AAMTTOT_HP',
                             'AAMTTOT_IP','AAMTTOT_IU','AAMTTOT_MP','AAMTTOT_OP', "PAS","H_CENSUS",
                             "H_METRO","ROSTSEX","H_AGE","Age_bin","Income_R","JobSatus","Health_status",
                             "cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","CS1YRWGT","D_MA",
                             "Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                             "Smoking","BMI_Class","RC1","RC2","RC3","CS1YRWGT_cost"))

print(paste(i))
print(colnames(ndata))

### removing missing data
ndata<-ndata[ndata$AAMTTOT_IP>0,]
min(ndata$AAMTTOT_IP)
ndata<-ndata[ndata$Spdegrcv_R!='Missing',]
ndata<-ndata[ndata$Spmarsta!="Missing",]

ndata<-ndata[ndata$Income_R!="Missing",]

ndata<-ndata[ndata$Race_origin!="Missing",]
ndata<-ndata[ndata$D_HHTOT._R!="FALSE",]

ndata<-ndata[ndata$Health_status!="missing",]
ndata<-ndata[ndata$JobSatus!="missing",]
ndata<-ndata[ndata$]

### log tranformation and checking the histogram
#hist(log(data$cost[data$cost !=-999&data$cost !=0]))
#hist(log(ndata_new$H_AGE))
###

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





ndata$H_INPSTY_class<-ifelse(ndata$H_INPSTY<=0, "NoVisits", ifelse(ndata$H_INPSTY==1,"1","Above2"))

ndata[ndata$H_INPSTY>1,]
ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_INPSTY_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                                                         "Smoking","BMI_Class"))

ndata_new<-ndata_new[!ndata_new$AAMTTOT_IP==0,]
ndata_new<-ndata_new[!is.na(ndata_new$AAMTTOT_IP),]
  print (nrow(ndata_new))
  ndata_new$H_IN
ndata_new$CS1YRWGT_cost
View(table(ndata_new$AAMTTOT_OP))
fit<-lm(log(ndata_new$AAMTTOT_IP) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
          +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
          +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
          +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
          +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
          +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
          +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
          +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
          +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
          +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
          +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily+
            +ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
          +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
          +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
          +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6
          +ndata_new$H_CENSUS.7+ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9
          +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight+
           ndata_new$BMI_Class.Normal+ndata_new$H_INPSTY_class.1+ndata_new$H_INPSTY_class.Above2+ndata_new$H_INPSTY_class.NoVisits,weights = as.numeric(ndata_new$CS1YRWGT_cost),data = ndata_new)
  name=paste("Model_2016",i,".csv")
  lmOut(fit,paste("C:/PAM",name,sep="/"))
}  
paste("C:/PAM","Model_2016",i,".csv",sep="/")

regression.function(data,'AAMTTOT_MP')
summary(fit)

lmOut(fit,"C:/Users/sushbiswal/Desktop/PAM/Model_revised_Aug18/Model_2012_Inpatient.csv")

name=paste("Model_2016_inpatientcosts",".csv")
lmOut(fit,paste("C:/Users/sushbiswal/Desktop/PAM",name,sep="/"))



######




ndata

ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_INPSTY_class","H_PMTVST_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R","Smoking","BMI_Class"))
ndata_new<-ndata_new[ndata_new$AAMTTOT_PM>0,]
min(ndata_new$AAMTTOT_PM)
print (nrow(ndata_new))
View(table(ndata_new$AAMTTOT_OP))
fit<-lm(log(ndata_new$AAMTTOT_PM) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
        +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
        +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
        +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
        +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
        +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
        +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
        +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
        +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
        +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
        +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily+
          +ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
        +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
        +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
        +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6
        +ndata_new$H_CENSUS.7+ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9+
        +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight
        +ndata_new$BMI_Class.Normal,weights = as.numeric(ndata_new$CS1YRWGT_cost),data = ndata_new)

lmOut(fit,"C:/Users/sushbiswal/Desktop/PAM/Model_revised_Aug18/Model_2012_prescribedmed.csv")
print(j)

#####

ndata$H_INPSTY_class<-ifelse(ndata$H_INPSTY<=0, "NoVisits", ifelse(ndata$H_INPSTY==1,"1","Above2"))
ndata$H_PMTVST_class<-ifelse(ndata$H_PMTVST<=0, "NoVisits", ifelse(ndata$H_PMTVST>0 & ndata$H_PMTVST<=4,"1--4",
                                                                   ifelse(ndata$H_PMTVST>4 & ndata$H_PMTVST<=9, "5--9", "Above10")))
ndata[ndata$H_PMTVST==0,]
ndata<-ndata[ndata$H_PMTCHO>0,]
ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_INPSTY_class","H_PMTVST_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R","Smoking","BMI_Class"))


head(ndata_new$H_PMTCHO)
nrow(ndata[!ndata$H_PMTCHO==0,])
print (nrow(ndata_new))
View(table(ndata_new$AAMTTOT_OP))

ndata$H_PMTVST_class
fit<-lm(log(ndata_new$H_PMTCHO) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
        +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
        +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
        +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
        +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
        +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
        +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
        +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
        +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
        +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
        +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily+
          +ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
        +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
        +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
        +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6
        +ndata_new$H_CENSUS.7+ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9+ndata_new$H_CENSUS.10
        +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight
        +ndata_new$BMI_Class.Normal+ndata_new$H_PMTVST_class.Above10
        +ndata_new$`H_PMTVST_class.1--4`+ndata_new$`H_PMTVST_class.5--9`
        ,weights = as.numeric(ndata_new$CS1YRWGT_cost),data = ndata_new)
name=paste("Model_2016",i,".csv")
lmOut(fit,paste("C:/PAM",name,sep="/"))
}  
paste("C:/PAM","Model_2016",i,".csv",sep="/")

regression.function(data,'AAMTTOT_MP')
summary(fit)

lmOut(fit,"C:/Users/sushbiswal/Desktop/PAM/Model_revised_Aug18/Model_2012_physiciancost.csv")

name=paste("Model_2016_physiciancosts",".csv")
lmOut(fit,paste("C:/Users/sushbiswal/Desktop/PAM",name,sep="/"))



### out of pockets cost 
nrow(data[!data$AAMTOOP_total==0 , ])
nrow(data[!data$AAMTOOP_PM==0 , ])

file$AAMTOOP_total=file$AAMTOOP_DU+file$AAMTOOP_FA+file$AAMTOOP_HH+file$AAMTOOP_HP+file$AAMTOOP_IP
+file$AAMTOOP_IU+file$AAMTOOP_MP+file$AAMTOOP_OP+file$AAMTOOP_PM




ndata<-subset(file, select=c('AAMTOOP_total','H_PMTCHO','H_PMTVST','H_INPSTY','H_INPDAY','H_OUTBIL' ,                                 
                             'AAMTTOT_PM','AAMTTOT_DU','AAMTTOT_FA','AAMTTOT_HH','AAMTTOT_HP','AAMTTOT_IP','AAMTTOT_IU','AAMTTOT_MP','AAMTTOT_OP', "PAS","H_CENSUS","H_METRO","ROSTSEX","H_AGE","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","CS1YRWGT","D_MA","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                             "Smoking","BMI_Class","RC1","RC2","RC3","CS1YRWGT_cost"))


###
### removing missing data

ndata<-ndata[ndata$Spdegrcv_R!='Missing',]
ndata<-ndata[ndata$Spmarsta!="Missing",]

ndata<-ndata[ndata$Income_R!="Missing",]

ndata<-ndata[ndata$Race_origin!="Missing",]
ndata<-ndata[ndata$D_HHTOT._R!="FALSE",]

ndata<-ndata[ndata$Health_status!="missing",]
ndata<-ndata[ndata$JobSatus!="missing",]

### log tranformation and checking the histogram
#hist(log(data$cost[data$cost !=-999&data$cost !=0]))
#hist(log(ndata_new$H_AGE))
###

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




ndata$H_INPSTY_class<-ifelse(ndata$H_INPSTY<=0, "NoVisits", ifelse(ndata$H_INPSTY==1,"1","Above2"))
ndata$H_PMTVST_class<-ifelse(ndata$H_PMTVST<=0, "NoVisits", ifelse(ndata$H_PMTVST>0 & ndata$H_PMTVST<=4,"1--4",
                                                                   ifelse(ndata$H_PMTVST>4 & ndata$H_PMTVST<=9, "5--9", "Above10")))
ndata=ndata[ndata$AAMTOOP_total>0,]
ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_INPSTY_class","H_PMTVST_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R","Smoking","BMI_Class"))


fit<-lm(log(ndata_new$AAMTOOP_total) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
        +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
        +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
        +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
        +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
        +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
        +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
        +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
        +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
        +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
        +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily+
          +ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
        +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
        +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
        +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6
        +ndata_new$H_CENSUS.7+ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9
        +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight
        +ndata_new$BMI_Class.Normal+ndata_new$H_PMTVST_class.Above10+ndata_new$H_INPSTY_class.Above2+ndata_new$H_INPSTY_class.1
        +ndata_new$H_INPSTY_class.NoVisits
        +ndata_new$`H_PMTVST_class.1--4`+ndata_new$`H_PMTVST_class.5--9`+ndata_new$H_PMTVST_class.NoVisits
        ,weights = as.numeric(ndata_new$CS1YRWGT_cost),data = ndata_new)

lmOut(fit,"C:/Users/sushbiswal/Desktop/PAM/Model_revised_Aug18/Model_2012_TotalOOP.csv")

summary(fit)


#### outpatient

ndata<-subset(file, select=c('AAMTTOT_OP','H_PMTCHO','H_PMTVST','H_INPSTY','H_INPDAY','H_OUTBIL' ,                                 
                             'AAMTTOT_PM','AAMTTOT_DU','AAMTTOT_FA','AAMTTOT_HH','AAMTTOT_HP',
                             'AAMTTOT_IP','AAMTTOT_IU','AAMTTOT_MP','AAMTTOT_OP', "PAS","H_CENSUS",
                             "H_METRO","ROSTSEX","H_AGE","Age_bin","Income_R","JobSatus","Health_status",
                             "cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","CS1YRWGT","D_MA",
                             "Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                             "Smoking","BMI_Class","RC1","RC2","RC3","CS1YRWGT_cost","H_OUTSW"))

print(paste(i))
print(colnames(ndata))

### removing missing data
ndata<-ndata[ndata$AAMTTOT_OP>0,]

ndata<-ndata[ndata$Spdegrcv_R!='Missing',]
ndata<-ndata[ndata$Spmarsta!="Missing",]

ndata<-ndata[ndata$Income_R!="Missing",]

ndata<-ndata[ndata$Race_origin!="Missing",]
ndata<-ndata[ndata$D_HHTOT._R!="FALSE",]

ndata<-ndata[ndata$Health_status!="missing",]
ndata<-ndata[ndata$JobSatus!="missing",]
ndata<-ndata[ndata$]

### log tranformation and checking the histogram
#hist(log(data$cost[data$cost !=-999&data$cost !=0]))
#hist(log(ndata_new$H_AGE))
###

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

ndata$H_INPSTY_class<-ifelse(ndata$H_INPSTY<=0, "NoVisits", ifelse(ndata$H_INPSTY==1,"1","Above2"))




ndata$H_OUTSW_class<-ifelse(ndata$H_OUTSW==0, "No", "Yes")

ndata[ndata$H_INPSTY>1,]
ndata_new <- dummy.data.frame(ndata, sep = ".",names = c("PAS","H_OUTSW_class","H_CENSUS","H_METRO","ROSTSEX","Age_bin","Income_R","JobSatus","Health_status","cancer","Race_origin","Spdegrcv_R","Spmarsta","D_HHTOT._R","D_MA1","Dual_Eligibility","Diabetes","Lungcond","Depression","Arthiritis","BloodPressure","HeartConditions","Dwelling_R",
                                                         "Smoking","BMI_Class"))


fit<-lm(log(ndata_new$AAMTTOT_OP) ~ ndata_new$`PAS.High Activation`+ndata_new$`PAS.Moderate Activation`+ndata_new$`PAS.Low Activation`
        +ndata_new$ROSTSEX.Female+ndata_new$ROSTSEX.Male+ndata_new$`Age_bin.75-84`+ndata_new$Age_bin.greaterthan85+ndata_new$`Age_bin.65-74`
        +ndata_new$`Income_R.25-49k`+ndata_new$`Income_R.50+`+ndata_new$Income_R.lessthan25k
        +ndata_new$Race_origin.BlackNonHispanic+ndata_new$Race_origin.Hispanic+ndata_new$Race_origin.OtherNonHispanic+ndata_new$Race_origin.WhiteNonHispanic
        +ndata_new$Spdegrcv_R.College+ndata_new$Spdegrcv_R.PostGraduate+ndata_new$`Spdegrcv_R.Some College`+ndata_new$`Spdegrcv_R.less than High School`+ndata_new$Spmarsta.Married
        +ndata_new$Spmarsta.Seperated+ndata_new$Spmarsta.Widowed+ndata_new$Spmarsta.NeverMarried
        +ndata_new$D_HHTOT._R.fourplus+ndata_new$D_HHTOT._R.twothree+ndata_new$D_HHTOT._R.oneperson+ndata_new$D_MA1.Yes+ndata_new$`Dual_Eligibility.Dual Eligible`
        +ndata_new$Diabetes.Yes+ndata_new$Lungcond.YES+ndata_new$Depression.Yes
        +ndata_new$Arthiritis.Yes+ndata_new$BloodPressure.Yes+ndata_new$HeartConditions.Yes+ndata_new$cancer.yes+ndata_new$Health_status.good
        +ndata_new$Health_status.excellent+ndata_new$Health_status.fair+ndata_new$Health_status.verygood+ndata_new$Health_status.verygood+ndata_new$Health_status.poor
        +ndata_new$JobSatus.yes+ndata_new$Dwelling_R.Onefamily+
          +ndata_new$Dwelling_R.Duplex+ndata_new$Dwelling_R.Mobile+ndata_new$Dwelling_R.Other
        +ndata_new$RC1+ndata_new$RC2+ndata_new$RC3
        +ndata_new$Smoking.Yes+ndata_new$H_METRO.Y+ndata_new$H_CENSUS.1+ndata_new$H_CENSUS.2
        +ndata_new$H_CENSUS.3+ndata_new$H_CENSUS.4+ndata_new$H_CENSUS.5+ndata_new$H_CENSUS.6
        +ndata_new$H_CENSUS.7+ndata_new$H_CENSUS.8+ndata_new$H_CENSUS.9
        +ndata_new$BMI_Class.Obese+ndata_new$BMI_Class.Overweight+
          ndata_new$BMI_Class.Normal,weights = as.numeric(ndata_new$CS1YRWGT_cost),data = ndata_new)

summary(fit)

lmOut(fit,"C:/Users/sushbiswal/Desktop/PAM/Model_revised_Aug18/Model_2012_outpatient.csv")
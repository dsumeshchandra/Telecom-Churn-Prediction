#Setting workind directory and initial data exploration
rm(list = ls())
setwd("D:/JigsawR/Capstone Project")
telecom=read.csv("sampletelecomfinal.csv",sep=",",header = T,na.strings = c(NA,""),stringsAsFactors = T)
names(telecom)
str(telecom)
summary(telecom)

#Library Import
options(scipen = 999)
library(data.table)
library(gains)
library(dplyr)
library(ROCR)
library(caret)
library(car)
##Data Quality Report

Variables<-names(telecom)
dqreport<-as.data.frame(Variables)
dqreport$DataType<-sapply(telecom,class)
dqreport$No_of_Records<-nrow(telecom)
dqreport$DataAvailable<-colSums(!is.na(telecom))
dqreport$AvailablePercentage<-round(colMeans(!is.na(telecom)),4)
dqreport$Missing<-colSums(is.na(telecom))
dqreport$MissingPercentage<-round(colMeans(is.na(telecom)),4)

for (i in 1:ncol(telecom)) {
  dqreport$UniqueRecords[i]<-length(unique(telecom[,i]))
  dqreport$Minimum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",min(telecom[,i],na.rm=T),0),2)
  dqreport$Maximum[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",max(telecom[,i],na.rm=T),0),2)
  dqreport$Mean[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",mean(telecom[,i],na.rm=T),0),2)
  dqreport$fifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.05,na.rm=T),0),2)
  dqreport$tenthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.10,na.rm=T),0),2)
  dqreport$twentyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.25,na.rm=T),0),2)
  dqreport$fiftythPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.50,na.rm=T),0),2)
  dqreport$seventyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.75,na.rm=T),0),2)
  dqreport$ninetythPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.90,na.rm=T),0),2)
  dqreport$ninetyfifthPercentile[i]<-round(ifelse(class(telecom[,i])=="integer"|class(telecom[,i])=="numeric",quantile(telecom[,i],p=0.95,na.rm=T),0),2)
}

write.csv(dqreport,"Data_Quality_Report.csv",row.names = T)

#-----------Exploratory Data Analysis-------------------------
#mou_Mean
summary(telecom$mou_Mean)
dat1<-telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat1$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$varname<-rep("mou_Mean",nrow(dat1))
ggplot(telecom,aes(x=mou_Mean))+geom_histogram(color="black",fill="white",binwidth = 100)+scale_x_continuous(breaks = seq(0,4000,400))+
  labs(title=" Distribution-Minutes of Use ")
ggplot(telecom,aes(y=mou_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat1,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="mou_Mean",y="Churn_%")# 

#tormrc_Mean
summary(telecom$totmrc_Mean)
dat2<-telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat2$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
ggplot(telecom,aes(x=totmrc_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+scale_x_continuous(breaks = seq(0,300,20))+
  labs(title=" Distribution-totmrc_Mean ")
ggplot(telecom,aes(y=totmrc_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat2,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="totmrc_Mean",y="Churn_%")# 

#rev_Range
summary(telecom$rev_Range)
dat3<-telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat3$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$varname<-rep("rev_Range",nrow(dat3))
ggplot(telecom,aes(y=rev_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat3,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="rev_Range",y="Churn_%") 

#mou_Range
summary(telecom$mou_Range)
dat4<-telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat4$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$varname<-rep("mou_Range",nrow(dat4))
ggplot(telecom,aes(y=mou_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat4,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="mou_Range",y="Churn_%")# 

#change_mou
summary(telecom$change_mou)
dat5<-telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat5$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$varname<-rep("change_mou",nrow(dat5))
ggplot(telecom,aes(x=change_mou))+geom_histogram(color="black",fill="white",binwidth = 10)+
  labs(title=" Distribution-change_mou ")
ggplot(telecom,aes(y=change_mou))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat5,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="change_mou",y="Churn_%")# 

#drop_blk_Mean
summary(telecom$drop_blk_Mean)
dat6<-telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat6$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))
ggplot(telecom,aes(x=drop_blk_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-drop_blk_Mean ")
ggplot(telecom,aes(y=drop_blk_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat6,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="drop_blk_Mean",y="Churn_%")# 

#drop_vce_Range
summary(telecom$drop_vce_Range)
dat7<-telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat7$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$varname<-rep("drop_vce_range",nrow(dat7))
ggplot(telecom,aes(x=drop_vce_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-drop_vce_Range ")
ggplot(telecom,aes(y=drop_vce_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat7,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="drop_vce_Range",y="Churn_%")# 

#owylis_vce_Range
summary(telecom$owylis_vce_Range)
dat8<-telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat8$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
ggplot(telecom,aes(x=owylis_vce_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-owylis_vce_Range ")
ggplot(telecom,aes(y=owylis_vce_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat8,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="owylis_vce_Range",y="Churn_%")# 

#mou_opkv_Range
summary(telecom$mou_opkv_Range)
dat9<-telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat9$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))
ggplot(telecom,aes(x=mou_opkv_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-mou_opkv_Range ")
ggplot(telecom,aes(y=mou_opkv_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat9,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="mou_opkv_Range",y="Churn_%")# 

#months
summary(telecom$months)
dat10<-telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat10$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$varname<-rep("months",nrow(dat10))
ggplot(telecom,aes(x=months))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-months ")
ggplot(telecom,aes(y=months))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat10,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="months",y="Churn_%")# 

#totcalls
summary(telecom$totcalls)
dat11<-telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat11$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$varname<-rep("totcalls",nrow(dat11))
ggplot(telecom,aes(x=totcalls))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-totcalls ")
ggplot(telecom,aes(y=totcalls))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat11,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="totcalls",y="Churn_%")# 

#income
summary(telecom$income)
telecom%>%count(churn,levels=income)%>%filter(churn==1)->datC1
datC1$N<-unclass(telecom%>%filter(income%in%datC1$levels)%>%count(income))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("income",nrow(datC1))
ggplot(telecom,aes(x=income))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-income ")
ggplot(telecom,aes(y=income))+geom_boxplot(outlier.color = "red",outlier.shape = 16) 
ggplot(datC1,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="income",y="Churn_%")# 

#eqpdays
summary(telecom$eqpdays)
dat12<-telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat12$N<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$varname<-rep("eqpdays",nrow(dat12))
ggplot(telecom,aes(x=eqpdays))+geom_histogram(color="black",fill="white",binwidth = 50)+labs(title=" Distribution-eqpdays ")
ggplot(telecom,aes(y=eqpdays))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat12,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="eqpdays",y="Churn_%")# 

#custcare_Mean
summary(telecom$custcare_Mean)
dat13<-telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat13$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$varname<-rep("custcare_Mean",nrow(dat13))
ggplot(telecom,aes(x=custcare_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-custcare_Mean ")
ggplot(telecom,aes(y=custcare_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat13,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="custcare_Mean",y="Churn_%")# 

#callwait_Mean
summary(telecom$callwait_Mean)
dat14<-telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat14$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$varname<-rep("callwait_Mean",nrow(dat14))
ggplot(telecom,aes(x=callwait_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-callwait_Mean ")
ggplot(telecom,aes(y=callwait_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat14,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="callwait_Mean",y="Churn_%")# 

#iwylis_vce_Mean
summary(telecom$iwylis_vce_Mean)
dat15<-telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat15$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
ggplot(telecom,aes(x=iwylis_vce_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-iwylis_vce_Mean ")
ggplot(telecom,aes(y=iwylis_vce_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat15,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="iwylis_vce_Mean",y="Churn_%")# 

#callwait_Range
summary(telecom$callwait_Range)
dat16<-telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat16$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$varname<-rep("callwait_Range",nrow(dat16))
ggplot(telecom,aes(x=callwait_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-callwait_Range ")
ggplot(telecom,aes(y=callwait_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat16,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="callwait_Range",y="Churn_%")# 

#ccrndmou_Range
summary(telecom$ccrndmou_Range)
dat17<-telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat17$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
ggplot(telecom,aes(x=ccrndmou_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-ccrndmou_Range ")
ggplot(telecom,aes(y=ccrndmou_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat17,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="ccrndmou_Range",y="Churn_%")# 

#adjqty
summary(telecom$adjqty)
dat18<-telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat18$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$varname<-rep("adjqty",nrow(dat18))
ggplot(telecom,aes(x=adjqty))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-adjqty ")
ggplot(telecom,aes(y=adjqty))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat18,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="adjqty",y="Churn_%")# 

#ovrrev_Mean
summary(telecom$ovrrev_Mean)
dat19<-telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat19$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
ggplot(telecom,aes(x=ovrrev_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-ovrrev_Mean ")
ggplot(telecom,aes(y=ovrrev_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat19,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="ovrrev_Mean",y="Churn_%")# 

#rev_Mean
summary(telecom$rev_Mean)
dat20<-telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat20$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$varname<-rep("rev_Mean",nrow(dat20))
ggplot(telecom,aes(x=rev_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-rev_Mean ")
ggplot(telecom,aes(y=rev_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat20,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="rev_Mean",y="Churn_%")# 

#ovrmou_Mean
summary(telecom$ovrmou_Mean)
dat21<-telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat21$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))
ggplot(telecom,aes(x=ovrmou_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-ovrmou_Mean ")
ggplot(telecom,aes(y=ovrmou_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat21,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="ovrmou_Mean",y="Churn_%")# 

#comp_vce_Mean
summary(telecom$comp_vce_Mean)
dat22<-telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat22$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))
ggplot(telecom,aes(x=comp_vce_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-comp_vce_Mean ")
ggplot(telecom,aes(y=comp_vce_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat22,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="comp_vce_Mean",y="Churn_%")# 

#plcd_vce_Mean
summary(telecom$plcd_vce_Mean)
dat23<-telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat23$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))
ggplot(telecom,aes(x=plcd_vce_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-plcd_vce_Mean ")
ggplot(telecom,aes(y=plcd_vce_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat23,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="plcd_vce_Mean",y="Churn_%")# 

#avg3mou
summary(telecom$avg3mou)
dat24<-telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat24$N<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$varname<-rep("avg3mou",nrow(dat24))
ggplot(telecom,aes(x=avg3mou))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avg3mou ")
ggplot(telecom,aes(y=avg3mou))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat24,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avg3mou",y="Churn_%")# 

#avgmou
summary(telecom$avgmou)
dat25<-telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat25$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$varname<-rep("avgmou",nrow(dat25))
ggplot(telecom,aes(x=avgmou))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avgmou ")
ggplot(telecom,aes(y=avgmou))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat25,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avgmou",y="Churn_%")# 

#avg6mou
summary(telecom$avg6mou)
dat26<-telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat26$N<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$varname<-rep("avg6mou",nrow(dat26))
ggplot(telecom,aes(x=avg6mou))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avg6mou ")
ggplot(telecom,aes(y=avg6mou))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat26,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avg6mou",y="Churn_%")# 

#avg3qty
summary(telecom$avg3qty)
dat27<-telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat27$N<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$varname<-rep("avg3qty",nrow(dat27))
ggplot(telecom,aes(x=avg3qty))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avg3qty ")
ggplot(telecom,aes(y=avg3qty))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat27,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avg3qty",y="Churn_%")# 

#avgqty
summary(telecom$avgqty)
dat28<-telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat28$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$varname<-rep("avgqty",nrow(dat28))
ggplot(telecom,aes(x=avgqty))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avgqty ")
ggplot(telecom,aes(y=avgqty))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat28,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avgqty",y="Churn_%")# 

#avg6qty
summary(telecom$avg6qty)
dat29<-telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat29$N<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$varname<-rep("avg6qty",nrow(dat29))
ggplot(telecom,aes(x=avg6qty))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avg6qty ")
ggplot(telecom,aes(y=avg6qty))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat29,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avg6qty",y="Churn_%")# 

#models
telecom%>%count(churn,levels=models)%>%filter(churn==1)->datC2
datC2$N<-unclass(telecom%>%filter(models%in%datC2$levels)%>%count(models))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("models",nrow(datC2))
ggplot(telecom,aes(x=models))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-models ")
ggplot(telecom,aes(y=models))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC2,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="models",y="Churn_%")# 

#hnd_price
telecom%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC3
datC3$N<-unclass(telecom%>%filter(hnd_price%in%datC3$levels)%>%count(hnd_price))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("hnd_price",nrow(datC3))
ggplot(telecom,aes(x=hnd_price))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-hnd_price ")
ggplot(telecom,aes(y=hnd_price))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC3,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="hnd_price",y="Churn_%")# 

#actvsubs
telecom%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC4
datC4$N<-unclass(telecom%>%filter(actvsubs%in%datC4$levels)%>%count(actvsubs))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("actvsubs",nrow(datC4))
ggplot(telecom,aes(x=actvsubs))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-actvsubs ")
ggplot(telecom,aes(y=actvsubs))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC4,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="actvsubs",y="Churn_%")# 

#uniqsubs
telecom%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC5
datC5$N<-unclass(telecom%>%filter(uniqsubs%in%datC5$levels)%>%count(uniqsubs))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("uniqsubs",nrow(datC5))
ggplot(telecom,aes(x=uniqsubs))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-uniqsubs ")
ggplot(telecom,aes(y=uniqsubs))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC5,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="uniqsubs",y="Churn_%")# 

#forgntvl
telecom%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC6
datC6$N<-unclass(telecom%>%filter(forgntvl%in%datC6$levels)%>%count(forgntvl))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("forgntvl",nrow(datC6))
ggplot(telecom,aes(x=forgntvl))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-forgntvl ")
ggplot(telecom,aes(y=forgntvl))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC6,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="forgntvl",y="Churn_%")# 

#opk_dat_Mean
summary(telecom$opk_dat_Mean)
dat30<-telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat30$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$varname<-rep("opk_dat_Mean",nrow(dat30))
ggplot(telecom,aes(x=opk_dat_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-opk_dat_Mean ")
ggplot(telecom,aes(y=opk_dat_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat30,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="opk_dat_Mean",y="Churn_%")# 

#age1
summary(telecom$age1)
dat31<-telecom%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat31$N<-unclass(telecom%>%mutate(dec=ntile(age1,n=10))%>%count(dec)%>%unname())[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$varname<-rep("age1",nrow(dat31))
ggplot(telecom,aes(x=age1))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-age1 ")
ggplot(telecom,aes(y=age1))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat31,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="age1",y="Churn_%")

#age2
summary(telecom$age2)
dat32<-telecom%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat32$N<-unclass(telecom%>%mutate(dec=ntile(age2,n=10))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$varname<-rep("age2",nrow(dat32))
ggplot(telecom,aes(x=age2))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-age2 ")
ggplot(telecom,aes(y=age2))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat32,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="age2",y="Churn_%")

#mtrcycle
telecom%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC7
datC7$N<-unclass(telecom%>%filter(mtrcycle%in%datC7$levels)%>%count(mtrcycle))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("mtrcycle",nrow(datC7))
ggplot(telecom,aes(x=mtrcycle))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-mtrcycle ")
ggplot(telecom,aes(y=mtrcycle))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC7,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="mtrcycle",y="Churn_%")# 

#truck
telecom%>%count(churn,levels=truck)%>%filter(churn==1)->datC8
datC8$N<-unclass(telecom%>%filter(truck%in%datC8$levels)%>%count(truck))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("truck",nrow(datC8))
ggplot(telecom,aes(x=truck))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-truck ")
ggplot(telecom,aes(y=truck))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(datC8,aes(x=levels,y=ChurnPerc))+geom_point(aes(color=ChurnPerc,size=ChurnPerc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="truck",y="Churn_%")# 

#retdays
summary(telecom$retdays)
dat33<-telecom%>%mutate(dec=ntile(retdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat33$N<-unclass(telecom%>%mutate(dec=ntile(retdays,n=10))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$varname<-rep("retdays",nrow(dat33))
ggplot(telecom,aes(x=retdays))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-retdays ")
ggplot(telecom,aes(y=retdays))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat33,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="retdays",y="Churn_%")# 

#roam_Mean
summary(telecom$roam_Mean)
dat34<-telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat34$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$varname<-rep("roam_Mean",nrow(dat34))
ggplot(telecom,aes(x=roam_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-roam_Mean ")
ggplot(telecom,aes(y=roam_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat34,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="roam_Mean",y="Churn_%")# 

#recv_sms_Mean
summary(telecom$recv_sms_Mean)
dat35<-telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat35$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$varname<-rep("recv_sms_Mean",nrow(dat35))
ggplot(telecom,aes(x=recv_sms_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-recv_sms_Mean ")
ggplot(telecom,aes(y=recv_sms_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat35,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="recv_sms_Mean",y="Churn_%")# 

#blck_dat_Mean
summary(telecom$blck_dat_Mean)
dat36<-telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat36$N<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$varname<-rep("blck_dat_Mean",nrow(dat36))
ggplot(telecom,aes(x=blck_dat_Mean))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-blck_dat_Mean ")
ggplot(telecom,aes(y=blck_dat_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat36,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="blck_dat_Mean",y="Churn_%")# 

#mou_pead_Mean
summary(telecom$mou_pead_Mean)
dat37<-telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat37$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$varname<-rep("mou_pead_Mean",nrow(dat37))
ggplot(telecom,aes(x=mou_pead_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-mou_pead_Mean ")
ggplot(telecom,aes(y=mou_pead_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat37,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="mou_pead_Mean",y="Churn_%")# 

#da_Mean
summary(telecom$da_Mean)
dat38<-telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat38$N<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$varname<-rep("da_Mean",nrow(dat38))
ggplot(telecom,aes(x=da_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-da_Mean ")
ggplot(telecom,aes(y=da_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat38,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="da_Mean",y="Churn_%")# 

#da_Range
summary(telecom$da_Range)
dat39<-telecom%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat39$N<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat39$churn_perc<-dat39$n/dat39$N
dat39$varname<-rep("da_Range",nrow(dat39))
ggplot(telecom,aes(x=da_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-da_Range ")
ggplot(telecom,aes(y=da_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat39,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="da_Range",y="Churn_%")# 

#datovr_Mean
summary(telecom$datovr_Mean)
dat40<-telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat40$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$varname<-rep("datovr_Mean",nrow(dat40))
ggplot(telecom,aes(x=datovr_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-datovr_Mean ")
ggplot(telecom,aes(y=datovr_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat40,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="datovr_Mean",y="Churn_%")# 

#datovr_Range
summary(telecom$datovr_Range)
dat40<-telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat40$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$varname<-rep("datovr_Range",nrow(dat40))
ggplot(telecom,aes(x=datovr_Range))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-datovr_Range ")
ggplot(telecom,aes(y=datovr_Range))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat40,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="datovr_Range",y="Churn_%")# 

#drop_dat_Mean
summary(telecom$drop_dat_Mean)
dat41<-telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat41$N<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$varname<-rep("drop_dat_Mean",nrow(dat41))
ggplot(telecom,aes(x=drop_dat_Mean))+geom_histogram(color="black",fill="white",binwidth = 1)+labs(title=" Distribution-drop_dat_Mean ")
ggplot(telecom,aes(y=drop_dat_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat41,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="drop_dat_Mean",y="Churn_%")# 

#drop_vce_Mean
summary(telecom$drop_vce_Mean)
dat42<-telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat42$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$varname<-rep("drop_vce_Mean",nrow(dat42))
ggplot(telecom,aes(x=drop_vce_Mean))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-drop_vce_Mean ")
ggplot(telecom,aes(y=drop_vce_Mean))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat42,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="drop_vce_Mean",y="Churn_%")# 

#adjmou
summary(telecom$adjmou)
dat43<-telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat43$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$varname<-rep("adjmou",nrow(dat43))
ggplot(telecom,aes(x=adjmou))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-adjmou ")
ggplot(telecom,aes(y=adjmou))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat43,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="adjmou",y="Churn_%")# 

#totrev
summary(telecom$totrev)
dat44<-telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat44$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$varname<-rep("totrev",nrow(dat44))
ggplot(telecom,aes(x=totrev))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-totrev ")
ggplot(telecom,aes(y=totrev))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat44,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="totrev",y="Churn_%")# 

#adjrev
summary(telecom$adjrev)
dat45<-telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat45$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$varname<-rep("adjrev",nrow(dat45))
ggplot(telecom,aes(x=adjrev))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-adjrev ")
ggplot(telecom,aes(y=adjrev))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat45,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="adjrev",y="Churn_%")# 

#avgrev
summary(telecom$avgrev)
dat46<-telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat46$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$varname<-rep("avgrev",nrow(dat46))
ggplot(telecom,aes(x=avgrev))+geom_histogram(color="black",fill="white",binwidth = 10)+labs(title=" Distribution-avgrev ")
ggplot(telecom,aes(y=avgrev))+geom_boxplot(outlier.color = "red",outlier.shape = 16) #outliers treatment required
ggplot(dat46,aes(x=dec,y=churn_perc))+geom_point(aes(color=churn_perc,size=churn_perc))+scale_x_continuous(breaks = seq(1,10,1))+
  scale_color_gradient(low = "blue",high = "red")+geom_smooth()+labs(title = "Customer Churn Event Rate",x="avgrev",y="Churn_%")# 

#asl_flag
telecom%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC9
datC9$N<-unclass(telecom%>%filter(asl_flag%in%datC9$levels)%>%count(asl_flag))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("asl_flag",nrow(datC9))

#prizm_social_one
telecom%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC10
datC10$N<-unclass(telecom%>%filter(prizm_social_one%in%datC10$levels)%>%count(prizm_social_one))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("prizm_social_one",nrow(datC10))

#area
telecom%>%count(churn,levels=area)%>%filter(churn==1)->datC11
datC11$N<-unclass(telecom%>%filter(area%in%datC11$levels)%>%count(area))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("area",nrow(datC11))

#refurb_new
telecom%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC12
datC12$N<-unclass(telecom%>%filter(refurb_new%in%datC12$levels)%>%count(refurb_new))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("refurb_new",nrow(datC12))

#hnd_webcap
telecom%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC13
datC13$N<-unclass(telecom%>%filter(hnd_webcap%in%datC13$levels)%>%count(hnd_webcap))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("hnd_webcap",nrow(datC13))

#marital
telecom%>%count(churn,levels=marital)%>%filter(churn==1)->datC14
datC14$N<-unclass(telecom%>%filter(marital%in%datC14$levels)%>%count(marital))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("marital",nrow(datC14))

#ethnic
telecom%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC15
datC15$N<-unclass(telecom%>%filter(ethnic%in%datC15$levels)%>%count(ethnic))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("ethnic",nrow(datC15))

#dwlltype
telecom%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC16
datC16$N<-unclass(telecom%>%filter(dwlltype%in%datC16$levels)%>%count(dwlltype))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("dwlltype",nrow(datC16))

#dwllsize
telecom%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC17
datC17$N<-unclass(telecom%>%filter(dwllsize%in%datC17$levels)%>%count(dwllsize))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("dwllsize",nrow(datC17))

#occu1
telecom%>%count(churn,levels=occu1)%>%filter(churn==1)->datC18
datC18$N<-unclass(telecom%>%filter(occu1%in%datC18$levels)%>%count(occu1))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("occu1",nrow(datC18))

#proptype
telecom%>%count(churn,levels=proptype)%>%filter(churn==1)->datC19
datC19$N<-unclass(telecom%>%filter(proptype%in%datC19$levels)%>%count(proptype))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("proptype",nrow(datC19))

#cartype
telecom%>%count(churn,levels=cartype)%>%filter(churn==1)->datC20
datC20$N<-unclass(telecom%>%filter(cartype%in%datC20$levels)%>%count(cartype))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("cartype",nrow(datC20))

#car_buy
telecom%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC20
datC20$N<-unclass(telecom%>%filter(car_buy%in%datC20$levels)%>%count(car_buy))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("car_buy",nrow(datC20))

## Feature transformation for Higher level categorical variables

area<-data.frame(round(table(telecom$area)/13259,3))
telecom1<-merge(telecom,area,by.x = "area",by.y = "Var1",all.x = T)
telecom1$area1<-telecom1$Freq
telecom1<-telecom1[,-80]

crclscod<- data.frame(round(table(telecom$crclscod)/13259,3))
telecom2<-merge(telecom1,crclscod,by.x = "crclscod",by.y = "Var1",all.x = T)
telecom2$crclscod1<-telecom2$Freq
telecom2<-telecom2[,-81]

cartype<- data.frame(round(table(telecom$cartype)/13259,3))
telecom3<- merge(telecom2,cartype,by.x = "cartype",by.y = "Var1",all.x=T)
telecom3$cartype1<-telecom3$Freq
telecom3<-telecom3[,-82]

csa<- data.frame(round(table(telecom$csa)/13259,3))
telecom4<- merge(telecom3,csa,by.x = "csa",by.y = "Var1",all.x = T)
telecom4$csa1<-telecom4$Freq
telecom4<-telecom4[,-83]

ethnic<-data.frame(round(table(telecom$ethnic)/13259,3))
telecom5<-merge(telecom4,ethnic,by.x = "ethnic",by.y = "Var1",all.x = T)
telecom5$ethnic1<-telecom5$Freq
telecom5<-telecom5[,-84]

occu1<-data.frame(round(table(telecom$occu1)/13259,3))
telecom6<-merge(telecom5,occu1,by.x = "occu1",by.y = "Var1",all.x = T)
telecom6$occu1.1<-telecom6$Freq 
telecom6<-telecom6[,-85]

proptype<-data.frame(round(table(telecom$proptype)/13259,3))
telecom7<-merge(telecom6,proptype,by.x = "proptype",by.y = "Var1",all.x = T)
telecom7$proptype1<-telecom7$Freq
telecom7<-telecom7[,-86]

dwllsize<-data.frame(round(table(telecom$dwllsize)/13259,3))
telecom8<-merge(telecom7,dwllsize,by.x = "dwllsize",by.y = "Var1",all.x = T)
telecom8$dwllsize1<-telecom8$Freq
telecom8<-telecom8[,-87]


#Missing value imputation for variable of Interest with higher missing value rate.
telecom8$retdays[is.na(telecom8$retdays)]<-0
telecom8$retdays<-ifelse(telecom8$retdays>0,1,0)
telecom8$dwllsize1[is.na(telecom8$dwllsize)]<-0
telecom8$proptype1[is.na(telecom8$proptype1)]<-0
telecom8$occu1.1[is.na(telecom8$occu1.1)]<-0
telecom8$ethnic1[is.na(telecom8$ethnic1)]<-0
telecom8$csa1[is.na(telecom8$csa1)]<-0
telecom8$cartype1[is.na(telecom8$cartype1)]<-0
telecom8$area1[is.na(telecom8$area1)]<-0
telecom<-telecom8

# Correcting DataTypes from Numeric to Factor.
telecom$actvsubs<-as.factor(telecom$actvsubs)
telecom$uniqsubs<-as.factor(telecom$uniqsubs)
telecom$forgntvl<-as.factor(telecom$forgntvl)
telecom$mtrcycle<- as.factor(telecom$mtrcycle)
telecom$truck<-as.factor(telecom$truck)
telecom$churn<- as.factor(telecom$churn)
telecom$models<-as.factor(telecom$models)
telecom$retdays<-as.factor(telecom$retdays)


#Droping Variable with higher missing value rate
drops=c("div_type","children","mailresp","solflag","wrkwoman","numbcars",
        "mailordr")
telecom=telecom[,!(names(telecom)%in%drops)]

#Spliting dataset into variable with Numeric and Categorical features
telecom_num= select_if(telecom,is.numeric)                
var_num<- names(telecom_num)
telecom_cat<- select_if(telecom,Negate(is.numeric))
var_chr<- names(telecom_cat)

#Outlier Treatment
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.1, .9), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

telecom$mou_Mean=capOutlier(telecom$mou_Mean)
telecom$totmrc_Mean=capOutlier(telecom$totmrc_Mean)
telecom$rev_Range=capOutlier(telecom$rev_Range)
telecom$mou_Range=capOutlier(telecom$mou_Range)
telecom$change_mou=capOutlier(telecom$change_mou)
telecom$drop_blk_Mean=capOutlier(telecom$drop_blk_Mean)
telecom$drop_vce_Range=capOutlier(telecom$drop_vce_Range)
telecom$owylis_vce_Range=capOutlier(telecom$owylis_vce_Range)
telecom$mou_opkv_Range=capOutlier(telecom$mou_opkv_Range)
telecom$custcare_Mean=capOutlier(telecom$custcare_Mean)
telecom$eqpdays=capOutlier(telecom$eqpdays)
telecom$callwait_Mean=capOutlier(telecom$callwait_Mean)
telecom$iwylis_vce_Mean=capOutlier(telecom$iwylis_vce_Mean)
telecom$callwait_Range=capOutlier(telecom$callwait_Range)
telecom$ccrndmou_Range=capOutlier(telecom$ccrndmou_Range)
telecom$adjqty=capOutlier(telecom$adjqty)
telecom$ovrrev_Mean=capOutlier(telecom$ovrrev_Mean)
telecom$rev_Mean=capOutlier(telecom$rev_Mean)
telecom$ovrmou_Mean=capOutlier(telecom$ovrmou_Mean)
telecom$comp_vce_Mean=capOutlier(telecom$comp_vce_Mean)
telecom$plcd_vce_Mean=capOutlier(telecom$plcd_vce_Mean)
telecom$avg3mou=capOutlier(telecom$avg3mou)
telecom$avgmou=capOutlier(telecom$avgmou)
telecom$avg3qty=capOutlier(telecom$avg3qty)
telecom$avgqty=capOutlier(telecom$avgqty)
telecom$avg6mou=capOutlier(telecom$avg6mou)
telecom$avg6qty=capOutlier(telecom$avg6qty)
telecom$opk_dat_Mean=capOutlier(telecom$opk_dat_Mean)
telecom$roam_Mean=capOutlier(telecom$roam_Mean)
telecom$recv_sms_Mean=capOutlier(telecom$recv_sms_Mean)
telecom$blck_dat_Mean=capOutlier(telecom$blck_dat_Mean)
telecom$mou_pead_Mean=capOutlier(telecom$mou_pead_Mean)
telecom$da_Mean=capOutlier(telecom$da_Mean)
telecom$da_Range=capOutlier(telecom$da_Range)
telecom$datovr_Mean=capOutlier(telecom$datovr_Mean)
telecom$datovr_Range=capOutlier(telecom$datovr_Range)
telecom$drop_dat_Mean=capOutlier(telecom$drop_dat_Mean)
telecom$drop_vce_Mean=capOutlier(telecom$drop_vce_Mean)
telecom$adjmou=capOutlier(telecom$adjmou)
telecom$totrev=capOutlier(telecom$totrev)
telecom$adjrev=capOutlier(telecom$adjrev)
telecom$avgrev=capOutlier(telecom$avgrev)


# Impute missing values both categorical and numerical 

for(k in names(telecom)){
  if (k %in% var_num) {
    #impute numeric variable with median
    med<-median(telecom[[k]], na.rm = T)
    set(x=telecom,which(is.na(telecom[[k]])),k ,med)
  }else if(k %in% var_chr){
    #Impute categorical variable with mode
    mode<-names(which.max(table(telecom[[k]])))
    set(x=telecom,which(is.na(telecom[[k]])),k,mode)
  }
}
colSums(is.na(telecom))

#Introducting derived variable to remove multicolinearity

telecom<-mutate(telecom,minpercall=avg3mou/avg3qty)
telecom$minpercall[is.na(telecom$minpercall)]<-0
telecom$minpercall[telecom$minpercall=="Inf"]<-0

telecom<-mutate(telecom,billminadjpercall=adjmou/adjqty)
telecom$billminadjpercall[is.na(telecom$billminadjpercall)]<-0
telecom$billminadjpercall[telecom$billminadjpercall=="Inf"]<-0

telecom$age1_1<-ifelse(telecom$age1==0,"Default",
                       ifelse(telecom$age1<=30,"Young",
                              ifelse(telecom$age1>30 & telecom$age1<=55,"Mid Age","Old")))
str(telecom$age1_1)

## Traing and Test set split
set.seed(0)
ind<-sample(nrow(telecom),0.70*nrow(telecom),replace = F)
train<-telecom[ind,]
test<-telecom[-ind,]
table(telecom$churn)
table(train$churn)/9281
table(test$churn)/3978


#Model Building

mod<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1-age1-age2
         -adjmou-adjqty, data=train,family = "binomial")
summary(mod)

mod1<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1
         -occu1.1-opk_dat_Mean-recv_sms_Mean-blck_dat_Mean-mou_pead_Mean-drop_dat_Mean
         -totmrc_Mean-da_Range-marital-hnd_webcap-refurb_new-rev_Range-age1-age2
         , data=train,family = "binomial")
summary(mod1)

mod2<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1
         -occu1.1-opk_dat_Mean-recv_sms_Mean-blck_dat_Mean-mou_pead_Mean-drop_dat_Mean
         -totmrc_Mean-da_Range-marital-hnd_webcap-refurb_new-rev_Range-drop_blk_Mean-
           drop_vce_Range-owylis_vce_Range-ethnic1-csa1-cartype1-datovr_Mean-age2-age1
         , data=train,family = "binomial")
summary(mod2)

mod3<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1
         -occu1.1-opk_dat_Mean-recv_sms_Mean-blck_dat_Mean-mou_pead_Mean-drop_dat_Mean
         -totmrc_Mean-da_Range-marital-hnd_webcap-refurb_new-rev_Range-drop_blk_Mean-
           drop_vce_Range-owylis_vce_Range-ethnic1-csa1-cartype1-datovr_Mean-age2--age1-
           actvsubs-datovr_Range-car_buy-truck-mtrcycle-roam_Mean-forgntvl-models-
           plcd_vce_Mean-mou_opkv_Range-income-totcalls-dwlltype-da_Mean-custcare_Mean
         , data=train,family = "binomial")
summary(mod3)

mod4<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1
         -occu1.1-opk_dat_Mean-recv_sms_Mean-blck_dat_Mean-mou_pead_Mean-drop_dat_Mean
         -totmrc_Mean-da_Range-marital-hnd_webcap-refurb_new-rev_Range-drop_blk_Mean-
           drop_vce_Range-owylis_vce_Range-ethnic1-csa1-cartype1-datovr_Mean-age2-age1-
           actvsubs-datovr_Range-car_buy-truck-mtrcycle-roam_Mean-forgntvl-models-
           plcd_vce_Mean-mou_opkv_Range-income-totcalls-dwlltype-da_Mean-custcare_Mean-
           callwait_Range-ovrrev_Mean-ccrndmou_Range-avgrev-totrev-adjrev-rev_Mean-area1
         -iwylis_vce_Mean-callwait_Mean-crclscod1-adjmou-adjqty-mou_Mean, data=train,family = "binomial")
summary(mod4)

mod<-glm(formula = churn~.-dwllsize-proptype-occu1-ethnic-csa-cartype-crclscod-area-Customer_ID
         -avg6qty-avg6mou-avg3mou-avg3qty-avgmou-avgqty-dwllsize1-proptype1
         -occu1.1-opk_dat_Mean-recv_sms_Mean-blck_dat_Mean-mou_pead_Mean-drop_dat_Mean
         -totmrc_Mean-da_Range-marital-hnd_webcap-refurb_new-rev_Range-drop_blk_Mean-
           drop_vce_Range-owylis_vce_Range-ethnic1-csa1-cartype1-datovr_Mean-age2-
           actvsubs-datovr_Range-car_buy-truck-mtrcycle-roam_Mean-forgntvl-models-
           plcd_vce_Mean-mou_opkv_Range-income-totcalls-dwlltype-da_Mean-custcare_Mean-
           callwait_Range-ovrrev_Mean-ccrndmou_Range-avgrev-totrev-adjrev-rev_Mean-area1
         -iwylis_vce_Mean-callwait_Mean-crclscod1-adjmou-adjqty-mou_Mean, data=train,family = "binomial")
summary(mod)
vif(mod)

mod1<-glm(formula = churn~mou_Range+change_mou+months+eqpdays+ovrmou_Mean+comp_vce_Mean+asl_flag
          +prizm_social_one+age1_1+hnd_price+uniqsubs+retdays+drop_vce_Mean+minpercall+billminadjpercall,
          data = train,family = "binomial")
summary(mod1)
vif(mod1)

# Final Training Model

#Dummy Variable Creation

train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)

train$uniqsubs_2<-ifelse(train$uniqsubs == "2", 1, 0)
test$uniqsubs_2<-ifelse(test$uniqsubs == "2", 1, 0)

train$uniqsubs_3<-ifelse(train$uniqsubs == "3", 1, 0)
test$uniqsubs_3<-ifelse(test$uniqsubs == "3", 1, 0)

train$uniqsubs_4<-ifelse(train$uniqsubs == "4", 1, 0)
test$uniqsubs_4<-ifelse(test$uniqsubs == "4", 1, 0)

train$uniqsubs_5<-ifelse(train$uniqsubs == "5", 1, 0) 
test$uniqsubs_5<-ifelse(test$uniqsubs == "5", 1, 0)

train$uniqsubs_6<-ifelse(train$uniqsubs == "6", 1, 0) 
test$uniqsubs_6<-ifelse(test$uniqsubs == "6", 1, 0)

train$uniqsubs_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$uniqsubs_7<-ifelse(test$uniqsubs == "7", 1, 0)

train$uniqsubs_9<-ifelse(train$uniqsubs == "9", 1, 0)
test$uniqsubs_9<-ifelse(test$uniqsubs == "9", 1, 0)

train$uniqsubs_12<-ifelse(train$uniqsubs == "12", 1, 0)
test$uniqsubs_12<-ifelse(test$uniqsubs == "12", 1, 0)


train$prizm_social_one_R<-ifelse(train$prizm_social_one=="R",1,0)
test$prizm_social_one_R<-ifelse(test$prizm_social_one=="R",1,0)

train$prizm_social_one_S<-ifelse(train$prizm_social_one=="S",1,0)
test$prizm_social_one_S<-ifelse(test$prizm_social_one=="S",1,0)

train$prizm_social_one_T<-ifelse(train$prizm_social_one=="T",1,0)
test$prizm_social_one_T<-ifelse(test$prizm_social_one=="T",1,0)

train$prizm_social_one_U<-ifelse(train$prizm_social_one=="U",1,0)
test$prizm_social_one_U<-ifelse(test$prizm_social_one=="U",1,0)

train$age1_Mid_Age<-ifelse(train$age1_1 == "Mid Age", 1, 0)
test$age1_Mid_Age<-ifelse(test$age1_1 == "Mid Age", 1, 0)

train$age1_Old<-ifelse(train$age1_1 == "Old", 1, 0)
test$age1_Old<-ifelse(test$age1_1 == "Old", 1, 0)

train$age1_Young<-ifelse(train$age1_1 == "Young", 1, 0)
test$age1_Young<-ifelse(test$age1_1 == "Young", 1, 0)


finalmod1<-glm(formula = churn~mou_Range+change_mou+months+eqpdays+ovrmou_Mean+comp_vce_Mean+asl_flag_Y
              +prizm_social_one_R+prizm_social_one_S+prizm_social_one_T+prizm_social_one_U+age1_Mid_Age
              +age1_Young+age1_Old+hnd_price+uniqsubs_2
              +uniqsubs_3+uniqsubs_4+uniqsubs_5+uniqsubs_6+uniqsubs_7+uniqsubs_9+uniqsubs_12
              +retdays+drop_vce_Mean+minpercall+billminadjpercall,
              data = train,family = "binomial")

summary(finalmod1)
vif(finalmod1)

finalmod<-glm(formula = churn~mou_Range+change_mou+months+eqpdays+ovrmou_Mean+comp_vce_Mean+asl_flag_Y
               +prizm_social_one_S+prizm_social_one_U+age1_Mid_Age
               +age1_Old+hnd_price+uniqsubs_2+uniqsubs_3+uniqsubs_4+
               +retdays+drop_vce_Mean+minpercall+billminadjpercall,
               data = train,family = "binomial")

summary(finalmod)
vif(finalmod)

# Prediction 
pred<-predict(finalmod,type = "response",newdata = test)
hist(pred)
pred1<-ifelse(pred>0.2391,1,0)
pred2<-as.factor(pred1)
test$churn1<-as.factor(test$churn)
confusionMatrix(pred2,test$churn1,positive = "1")
pred<-prediction(pred,test$churn)
eval<-performance(pred,"acc")
plot(eval)
abline(h=0.776,v=0.63)

#Identifying best values
max<- which.max(slot(eval,"y.values")[[1]])
which.max(slot(eval,"y.values")[[1]])

acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]

#Roc curve
roc<-performance(pred,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)
abline(h=0.778,v=0.63)

#Area Under Curve
auc<- performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,2)
legend(.0,.9,auc,title = "AUC")

#AUC is 62.77% which is more than 50% 
# Also the curve seems to be well above the grey line.
# So the model seems to be ok and is acceptable.

# Identying customers highly likely to churn
test$prob<-predict(finalmod,type="response",newdata=test)
quantile(test$prob,prob=seq(0,1,0.1))

#Top 40% of the probability scores lie between 0.21123431 and 0.27366707
#We can use this probablity to extract the data of customers who are highly likely to churn.

#Feature Importance
vimp=varImp(finalmod)

# Top 5 important factors affecting churn rate are:
### a. retdays            with beta coefficient of 7.578563
### b. eqpdays            with beta coefficient of 6.638120
### c. comp_vce_Mean      with beta coefficient of 6.619381
### d. minpercall         with beta coefficient of 6.358806
### e. ovrmou_Mean        with beta coefficient of 4.806102

# What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:
#Targeted customers will be identified as follows:
predx<-predict(finalmod, type = "response", newdata = test)
quantile(predx, probs= seq(0,1,0.1))
test$churn_prob<-ifelse(predx< 0.20,"Low_Score",
                        ifelse(predx<0.20 & predx<=0.30, "Mid_Score","High_Score"))

str(test$totrev)
quantile(test$totrev, probs= seq(0,1,0.1))

test$Rev_Level<- ifelse(test$totrev<562.000, "Low_Revenue",
                        ifelse(test$totrev>=562.000 & test$totrev<=1100.000,"Medium_Revenue","High_Revenue"))
Target1<-test[test$churn_prob=="High_Score" &
                test$Rev_Level=="High_Revenue", "Customer_ID"]

Target1<-as.data.frame(Target1)
write.csv(Target1,"High_Score_High_Revenue_Customer.csv")



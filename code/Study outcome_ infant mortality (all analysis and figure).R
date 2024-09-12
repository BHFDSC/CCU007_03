rm(list = ls())

# first load the study cohort (all patients)
mydata=read.csv("D:/PhotonUser/My Files/Home Folder/Excel data/CHD summary/AllCHD.csv")
allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","AOS","COA","VSD")
CHDsubtypes_all=NULL
CHDsubtypes_all[[1]]="HLHS"
CHDsubtypes_all[[2]]=c("Double inlet ventricle","Tricuspid atresia")

CHDsubtypes_all[[3]]=c("Complex TGA & pulmonary stenosis","Complex TGA without pulmonary stenosis","TGA with intact ventricular septum")
CHDsubtypes_all[[4]]=c("Pulmonary atresia & VSD","Pulmonary atresia with intact ventricular septum")
CHDsubtypes_all[[5]]=c("Tetralogy AVSD","Unbalanced AVSD","Complete AVSD")

CHDsubtypes_all[[6]]=c("Tetralogy absent pulmonary valve","Tetralogy with DORV","Standard tetralogy")
CHDsubtypes_all[[7]]=c("Aortic stenosis & muti-level left heart obstruction","Isolated aortic stenosis")
CHDsubtypes_all[[8]]=c("Coarctation plus VSD","Isolated coarctation")
CHDsubtypes_all[[9]]=c("Multiple VSD","Isolated VSD")

mydata$CHDtype <- factor(mydata$CHDtype, levels=allCHDs)
mydata$CHDsubtype <- factor(mydata$CHDsubtype, 
                            levels = c(CHDsubtypes_all[[1]],CHDsubtypes_all[[2]],CHDsubtypes_all[[3]],
                                       CHDsubtypes_all[[4]],CHDsubtypes_all[[5]],CHDsubtypes_all[[6]],
                                       CHDsubtypes_all[[7]],CHDsubtypes_all[[8]],CHDsubtypes_all[[9]]))

#################################################################################### 
############################ Figure: infant mortality by era############################ 
#################################################################################### 

Mortalityrate1year_all<-matrix(NA,length(CHDtype),4)

colnames(Mortalityrate1year_all)=c("baseline","transition","lockdown","lockdown lifted")
rownames(Mortalityrate1year_all)=allCHDs

points_size<-1.2
diff=0.7
cex_axis<-1
cex_lab<-1.2
cex_axis_lab=1
size_ageband=0.85

outline_show=F
box_width=1.2
cex_lab_era=1


col1=grey(0.3)
col2=rgb(79,129,189,max=255)
col3=rgb(192,80,77,max=255)
col4="darkorange3"

col_all=c(col1,col2,col3,col4)
start=seq.int(1, 50,6)
plot(NULL,xlim=c(0.5,52),ylim=c(0,60),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,60,10),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=start+1.5,labels=alldiagnosis_names, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Mortality rate at age 1 year (%)", side=2, line=2.5,cex=cex_lab) 

for(i in 1:length(CHDtype)){

  df=mydata[which(mydata$CHDtype %in% allCHDs[i]),]
  
  death1<-NULL
  for(j in 1:4){
    fit=survfit(Surv(ageatlastknownstatus, lastknownstatus)~1, data=df[which(df$pat_era==j),])
    
    A=summary(fit, times=c(1))
    rate=100-A$surv*100
    upper=100-A$lower*100
    lower=100-A$upper*100
    Mortalityrate1year_all[i,j]=c(rate,lower,upper)
    
    lines(rep(start[i]+(j-1),2),c(lower,upper),lty=1, col=col_all[j],lwd=2)
    points(x=start[i]+(j-1),y=rate, pch=19, col=col_all[j])
    
  }
  
}
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3,bty="n")


#################################################################################### 
############################ multivariate  modelling (logistic regression)############################ 
#################################################################################### 
df=mydata 

df=df[-which(df$White==2 | df$Deprived==2),] # complete data analysis
length(which(df$death1==1)) # 218

model <- glm( death1 ~factor(pat_era) +  factor(White) +factor(Deprived)+factor(SexMale)+ relevel(CHDtype, ref = "TOF") +factor(patCongDowns) + factor(Premat), data = df, family = binomial(link = "logit"))
summary(model ,se = "boot",R=5000) # modelling results, using bootstrap to compute the confidence interval 
# of note, one need to use exp() to compute the odds ratio



#subgroup analysis:  test the interaction between the era and social factors 
model1 <- glm( death1 ~factor(pat_era):factor(White)+ factor(pat_era) +  factor(White) +factor(Deprived)+factor(SexMale)+ relevel(CHDtype, ref = "TOF") +factor(patCongDowns) + factor(Premat), data = df, family = binomial(link = "logit"))
anova(model, model1,test="LR") 

model2 <- glm( death1 ~factor(pat_era):factor(Deprived)+ factor(pat_era) +  factor(White) +factor(Deprived)+factor(SexMale)+ relevel(CHDtype, ref = "TOF") +factor(patCongDowns) + factor(Premat), data = df, family = binomial(link = "logit"))
anova(model, model2,test="LR")

model3<- glm( death1 ~factor(pat_era):factor(SexMale)+ factor(pat_era) +  factor(White) +factor(Deprived)+factor(SexMale)+ relevel(CHDtype, ref = "TOF") +factor(patCongDowns) + factor(Premat), data = df, family = binomial(link = "logit"))
anova(model, model3,test="LR") 


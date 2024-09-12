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
###########Figure: hospital resource utilization before age 1-year by era and by CHD type,  Wilcoxon rank sum test was performed at the same time 
#################################################################################### 
# significance levels 
showpvalue=function(p_orig){
  if(p_orig>=0.05){pround=round(p_orig,2);pi=paste0("p=",pround)}
  if(p_orig<0.05 & p_orig>0.01){pround=round(p_orig,2);pi=paste0("p=",pround,"*")}
  if(p_orig<=0.01 & p_orig>0.001){pround=round(p_orig,3);pi=paste0("p=",pround,"**")}
  if(p_orig<=0.001){pi=paste0("p<0.001***")}
  
  return(pi)
}

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
start=seq.int(1, 41,5)


#first, all hospital days . Welsh patients need to be removed because we don't have their outpatient data
plot(NULL,xlim=c(0,44),ylim=c(0,365),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,400,50),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=start+1.5,labels=alldiagnosis_names, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Days", side=2, line=2.5,cex=cex_lab) 
mtext("(a) All hospitalisation (days)", line=0.8,side=3, cex=cex_lab) 
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3,bty="n")
Alldayspavlues=matrix(NA,length(all_CHD),3)

for (i in 1:9){
  df=mydata[which(mydata$CHDtype %in% allCHDs[i]),]
  
  ALL<-NULL
  for(j in 1:4){
    ALL[[j]]=mydata$AllDaysbyYear1[which(df$pat_era==j & df$patWales==0)] 
    boxplot(ALL[[j]],at=start[i]+(j-1),yaxt='n', xaxt='n',add=T, boxwex = box_width,
            pch=19, cex=0.3,col=col_all[j],outline=outline_show)
    if(j %in% c(2,3,4)){
      test= wilcox.test(x=ALL[[j]], y =ALL[[1]],alternative = c( "two.sided") )
      Alldayspavlues[i,(j-1)]=showpvalue(test$p.value)
      
    }
  }
}

#then inpatient days only 
plot(NULL,xlim=c(0,44),ylim=c(0,300),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,400,50),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=start+1.5,labels=alldiagnosis_names, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Days", side=2, line=2.5,cex=cex_lab) 
mtext("(b) Inpatient hospitalisation (days)", line=0.8,side=3, cex=cex_lab) 
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3,bty="n")

IPdayspavlues=matrix(NA,9,3)

for (i in 1:9){
  df=mydata[which(mydata$CHDtype %in% allCHDs[i]),]
  
  IP<-NULL
  for(j in 1:4){
    IP[[j]]=df$IPDaysbyYear1[which(df$pat_era==j )]
    boxplot(IP[[j]],at=start[i]+(j-1),yaxt='n', xaxt='n',add=T, boxwex = box_width,
            pch=19, cex=0.3,col=col_all[j],outline=outline_show)
    if(j %in% c(2,3,4)){
      test= wilcox.test(x=IP[[j]], y =IP[[1]],alternative = c( "two.sided") )
      IPdayspavlues[i,(j-1)]=showpvalue(test$p.value)
    }
  }
}

#lastly outpatient days only . Welsh patients need to be removed because we don't have their outpatient data
plot(NULL,xlim=c(0,44),ylim=c(0,70),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,80,10),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=start+1.5,labels=alldiagnosis_names, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Days", side=2, line=2.5,cex=cex_lab) 
mtext("(c) Outpatient hospitalisation (days)", line=0.8,side=3, cex=cex_lab) 
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3,bty="n")

OPdayspavlues=matrix(NA,9,3)

for (i in 1:9){

  df=mydata[which(mydata$CHDtype %in% allCHDs[i]),]

  for(j in 1:4){
    OP[[j]]=df$OPDaysbyYear1[which(df$pat_era==j & df$patWales==0)]
    boxplot(OP[[j]],at=start[i]+(j-1),yaxt='n', xaxt='n',add=T, boxwex = box_width,
            pch=19, cex=0.3,col=col_all[j],outline=outline_show)
    if(j %in% c(2,3,4)){
      test= wilcox.test(x=OP[[j]], y =OP[[1]],alternative = c( "two.sided") )
      OPdayspavlues[i,(j-1)]=showpvalue(test$p.value)
    } 
  }
}




#################################################################################### 
############################ multivariate  modelling (quantile regression by median)############################ 
#################################################################################### 
library(quantreg) 

# we model days spent at home in infancy (i.e., 365 days - total days spent as an inpatient; patients who died before age 1-year were assigned as 0 days at home as the worst outcome). 
mydata$nonIPdaysathome=365-mydata$IPDaysbyYear1
mydata$nonIPdaysathome[which(mydata$death1==1)]=0

# remove missing data, we use complete data analysis
df=mydata[-which(mydata$White==2 | mydata$Deprived==2),] 

df$CHDsubtype=as.factor(df$CHDsubtype)
df$patimd_2019_quintiles=as.factor(df$patimd_2019_quintiles)
ref_CHDsubtype="Standard tetralogy" 


model=rq(nonIPdaysathome ~ factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+  factor(SexMale)+ relevel(CHDsubtype, ref = ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
summary(model ,se = "boot",R=5000) # modelling results, using bootstrap to compute the confidence interval 



#subgroup analysis:  test the interaction between the era and social factors 
model1=rq(nonIPdaysathome ~factor(pat_era):factor(ethnicity)+ factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ 
            factor(SexMale)+ relevel(CHDsubtype, ref = ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
anova(model, model1, se = "boot") 

model2=rq(nonIPdaysathome ~factor(pat_era):relevel(patimd_2019_quintiles, ref = 5)+ factor(pat_era) + factor(ethnicity) 
          +relevel(patimd_2019_quintiles, ref = 5)+  factor(SexMale)+ relevel(CHDsubtype, ref = ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
anova(model, model2, se = "boot")

model3=rq(nonIPdaysathome ~factor(pat_era):factor(SexMale)+ factor(pat_era) + factor(ethnicity) +
            relevel(patimd_2019_quintiles, ref = 5)+  factor(SexMale)+ relevel(CHDsubtype, ref = ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
anova(model, model3, se = "boot") 



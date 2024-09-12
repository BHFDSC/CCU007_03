rm(list = ls())

# first load the study cohort (all patients)
mydata=read.csv("D:/PhotonUser/My Files/Home Folder/Excel data/CHD summary/AllCHD.csv")

# study outcome 1: age at palliative stage 1
mydata$patAge_model1=round(365.25*mydata$patAge1)

# study outcome 2: age at palliative stage 2 or reparative procedure 
mydata$patAge_model2=NA
# if patients who had both a reparative procedure and a palliative stage 2, and their first occurring procedures were used.
for(jj in 1:nrow(mydata)){
  if(!(is.na(mydata$patAge2[jj]) & is.na(mydata$patAge4[jj]))){
    mydata$patAge_model2[jj]=round(365.25*min(mydata$patAge2[jj],mydata$patAge4[jj],na.rm=T))}
}

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
###########Figure: treatment pathway age by era and by CHD type,  Wilcoxon rank sum test was performed at the same time 
#################################################################################### 

# significance levels 
showpvalue=function(p_orig){
  if(p_orig>=0.05){pround=round(p_orig,2);pi=paste0("p=",pround)}
  if(p_orig<0.05 & p_orig>0.01){pround=round(p_orig,2);pi=paste0("p=",pround,"*")}
  if(p_orig<=0.01 & p_orig>0.001){pround=round(p_orig,3);pi=paste0("p=",pround,"**")}
  if(p_orig<=0.001){pi=paste0("p<0.001***")}
  
  return(pi)
}


pat_era=1:4

points_size<-1.2
diff=0.7
cex_axis<-1
cex_lab<-1.2
cex_axis_lab=0.85
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

# palliative stage 1 figure
CHDwithstage1=c("HLHS","FUH","PA","AVSD","TOF","VSD")

stage1start=start[1:length(CHDwithstage1)]
plot(NULL,xlim=c(0,max(stage1start)+4),ylim=c(0,300),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,300,50),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=stage1start+2,labels=CHDwithstage1, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Days", side=2, line=2.5,cex=cex_lab) 
mtext("(a) Age of palliative stage 1", line=0.8,side=3, cex=cex_lab) 
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3)

stage1pavlues=matrix(NA,length(CHDwithstage1),3)
for (i in 1:length(CHDwithstage1)){

  df=mydata[which(mydata$CHDtype %in% CHDwithstage1[i]),]
  Age<-NULL
  for(j in 1:4){
    Age[[j]]=round(365.25*df$patAge1[which(mydata$pat_era==j)])
    boxplot(Age[[j]],at=stage1start[i]+(j-1),yaxt='n', xaxt='n',add=T, boxwex = box_width,
            pch=19, cex=0.3,col=col_all[j],outline=outline_show)
    
    if(j>1){test= wilcox.test(x=Age[[j]], y =Age[[1]],alternative = c( "greater") ); stage1pavlues[i,(j-1)]=showpvalue(test$p.value)}

  }
}

# palliative stage 2 and reparative procedure figure

##############################################################
plot(NULL,xlim=c(0,44),ylim=c(0,300),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, main="")
axis(2, at=seq(0,300,50),labels=T, cex.axis=cex_axis_lab,las=2)
axis(1, at=start+2,labels=allCHDs, 
     cex.axis=cex_axis_lab,col.ticks='white')
mtext("Days", side=2, line=2.5,cex=cex_lab) 
mtext("Age of the palliative stage 2 or reparative procedure", line=0.8,side=3, cex=cex_lab) 
legend("topright", c("Pre-pandemic baseline","Transition period","Restriction period","Post restriction period"),col=col_all,lty=1,lwd=3)

repairpavlues=matrix(NA,length(allCHDs),3)

for (i in 1:9){
  df=mydata[which(mydata$CHDtype %in% allCHDs[i]),]
  df$patAge_model2=df$patAge_model2/365.25 # convert to years
 
  Age<-NULL
  for(j in 1:4){
    Age[[j]]=df$patAge_model2[which(df$pat_era==j)]
    boxplot(Age[[j]],at=start[i]+(j-1),yaxt='n', xaxt='n',add=T, boxwex = box_width,
            pch=19, cex=0.3,col=col_all[j],outline=outline_show)
    if(j>1){test= wilcox.test(x=Age[[j]], y =Age[[1]],alternative = c( "greater") ); repairpavlues[i,(j-1)]=showpvalue(test$p.value)}
    
  }
}


#################################################################################### 
############################ multivariate  modelling (quantile regression by median)############################ 
#################################################################################### 
library(quantreg) 

ref_CHDsubtype="Standard tetralogy"

############################palliative stage 1 model
df=mydata[which(!is.na(mydata$patAge_model1)),] 
# remove missing data, we use complete data analysis
df=df[-which(df$White==2 | df$Deprived==2),] 

# remove CHD subtype with small sample size (<10)
A=table(df$CHDsubtype)
removed_diagnosis=names(A)[which(as.numeric(A)<10)]  
index_removed=which(df$CHDsubtype%in%removed_diagnosis) 

df=df[-index_removed,]


df$patimd_2019_quintiles=as.factor(df$patimd_2019_quintiles)
# patimd_2019_quintiles (IMD): 1(most deprived)-5(least deprived)
df$CHDsubtype=as.factor(df$CHDsubtype)

model=rq(patAge_model1 ~ factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
summary(model ,se = "boot",R=5000) # modelling results, using bootstrap to compute the confidence interval 

#subgroup analysis:  test the interaction between the era and social factors 
model1=rq(patAge_model1 ~ factor(pat_era):factor(ethnicity)+factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , data = df, tau = 0.5,method="fn")
anova(model, model1, se = "boot")

model2=rq(patAge_model1 ~ factor(pat_era):relevel(patimd_2019_quintiles, ref = 5)+
            factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat)
           ,data = df, tau = 0.5,method="fn")
anova(model, model2, se = "boot") 


model3=rq(patAge_model1 ~ factor(pat_era):factor(SexMale)+
            factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat),
          data = df, tau = 0.5,method="fn")
anova(model, model3, se = "boot") 


############################ palliative stage 2 and reparative procedure model

df=mydata[which(!is.na(mydata$patAge_model2)),] 
A=table(df$CHDsubtype) # all >10
df=df[-which(df$White==2 | df$Deprived==2),] 

df$patimd_2019_quintiles=as.factor(df$patimd_2019_quintiles)
df$CHDsubtype=as.factor(df$CHDsubtype)
model=rq(patAge_model2 ~ 
        factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , 
         data = df, tau = 0.5,method="fn")
summary(model ,se = "boot",R=5000) # modelling results, using bootstrap to compute the confidence interval 

#subgroup analysis:  test the interaction between the era and social fatcors 
model1=rq(patAge_model2 ~ factor(pat_era):factor(ethnicity)+
            factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , 
          data = df, tau = 0.5,method="fn")
anova(model, model1, se = "boot")

model2=rq(patAge_model2 ~ factor(pat_era):relevel(patimd_2019_quintiles, ref = 5)+
            factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , 
          data = df, tau = 0.5,method="fn")
anova(model, model2, se = "boot") 

model3=rq(patAge_model2 ~ factor(pat_era):factor(SexMale)+
            factor(pat_era) + factor(ethnicity) +relevel(patimd_2019_quintiles, ref = 5)+ factor(SexMale)+ relevel(CHDsubtype, ref =ref_CHDsubtype)+factor(patCongDowns) + factor(Premat) , 
          data = df, tau = 0.5,method="fn")
anova(model, model3, se = "boot") 


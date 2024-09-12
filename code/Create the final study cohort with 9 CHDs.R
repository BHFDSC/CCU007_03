rm(list = ls())

########### extract risk factor and study outcome from each CHD dataset
Extract_mydata<-function(mydata,CHDtype){
  mydata$CHDtype=CHDtype
  vars=c("patid","patWales", 
         "pat_era",#exposure (pandemic era) 
         "SexMale", "ethnicity","patimd_2019_quintiles",  #social factors (risk factors)
         "CHDtype","CHDsubtype","diagsubgroup","Premat","patDowns","patcongcomorb" , # case mix (risk factors)
         "lastknownstatus","ageatlastknownstatus" , # vital status
         "patAge1","patAge2" ,"patAge4", # outcome: age at treatment pathway
         "AllDaysbyYear1","IPDaysbyYear1","OPDaysbyYear1", # outcome: hospital resource utilization during the 1st year of life
         "death1" # outcome: infant mortality
         )
  
  subset=mydata[,vars]
  return(subset)
}

CHDsubgroups_all=NULL
CHDsubgroups_all[[1]]="HLHS"
CHDsubgroups_all[[2]]=c("Double inlet ventricle","Tricuspid atresia")

CHDsubgroups_all[[3]]=c("Complex TGA & pulmonary stenosis","Complex TGA without pulmonary stenosis","TGA with intact ventricular septum")
CHDsubgroups_all[[4]]=c("Pulmonary atresia & VSD","Pulmonary atresia with intact ventricular septum")
CHDsubgroups_all[[5]]=c("Tetralogy AVSD","Unbalanced AVSD","Complete AVSD")

CHDsubgroups_all[[6]]=c("Tetralogy absent pulmonary valve","Tetralogy with DORV","Standard tetralogy")
CHDsubgroups_all[[7]]=c("Aortic stenosis & muti-level left heart obstruction","Isolated aortic stenosis")
CHDsubgroups_all[[8]]=c("Coarctation plus VSD","Isolated coarctation")
CHDsubgroups_all[[9]]=c("Multiple VSD","Isolated VSD")

########### create a final study cohort and assign CHD type and subgroups (case mix) 

allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","AOS","COA","VSD")
mydata_all=NULL
for (i in 1:9){
  CHDtype=allCHDs[i]
  mydata<-read.csv(paste0("D:/PhotonUser/My Files/Home Folder/Data files/",CHDtype,"/",CHDtype,"_finalPatientLevel.csv"))
  
  if(CHDtype=="AVSD") # remove partial AVSD, because they had their treatment pathway order
  {
    mydata=mydata[-which(mydata$diagsubgroup==3)]
    mydata$diagsubgroup[which(mydata$diagsubgroup==4)]=3
  }
  mydata$CHDsubtype=NA
  for(j in 1:max(mydata$diagsubgroup)){
    index=which(mydata$diagsubgroup==j)
    mydata$CHDsubtype[index]=CHDsubgroups_all[[i]][j]
  }
  
  mydata_all<-rbind(mydata_all,Extract_mydata(mydata,CHDtype))
}


############ main exposure
#pandemic era: 1 (baseline) 2(transition) 3(restriction) 4(post restriction)
mydata_all$pat_era 

############ social factors
#gender
mydata_all$SexMale

#ethnicity 1 (White) 2(Black) 3 (Asian) 4(mixed/others) 5 (missing)
mydata_all$White=0 
mydata_all$White[which(mydata_all$ethnicity %in% c(1))]=1 #white
mydata_all$White[which(mydata_all$ethnicity %in% c(5))]=2 #missing

#deprivation:
#patimd_2019_quintiles (IMD) 1(most deprived)-5(least deprived); 6 (missing)
mydata_all$Deprived=0 #least deprived areas 
mydata_all$Deprived[which(mydata_all$patimd_2019_quintiles %in% c(1,2))]=1 # most deprived areas (IMD 1-2) 
mydata_all$Deprived[which(mydata_all$patimd_2019_quintiles %in% c(6))]=2 #missing


###### other risk factors
# casemix: group Downs to congenital comorbidity 
mydata_all$patCongDowns=0 
mydata_all$patCongDowns[which(mydata_all$patDowns==1 | mydata_all$patcongcomorb==1)]=1 



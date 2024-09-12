rm(list = ls())

# Forest plot for all modelling outcomes related to birth era  (Figure 5)



birth_era=c("Transition period","Restriction period","Post restriction period")

cex_lab=1
cex_text=0.85


par(mar=c(2.5,1,1,0.5), mgp=c(1,.4,0), tck=-.01)
par(fig=c(0,0.5,0.5,1), new=F)
textposition=0.4

##################### pathway treatment age mode 1
df=stage1model # load palliative stage 1 model results, see how to compute it in pathway age analysis R file. 
# df is of size  length(birth_era) 

plot(NULL,xlim=c(-10,10),ylim=c(0.8,4),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, bty="n",main="")
axis(1, at=seq(-10,10,2),labels=T, cex.axis=1)
lines(rep(0,2),c(0,3.2),lwd=2,lty=2,col=gray(0.4))
y_pos=3:1

for(i in 1:3){
  lines(df[i,3:4],rep(y_pos[i],2),lwd=2,col=gray(0.4))
  points(round(df[i]),round(y_pos[i]),pch=19,col=gray(0.1))
  legend(-11,y_pos[i]+textposition,birth_era[i],bty="n",cex=cex_text)
  legend(2.5,y_pos[i]+textposition,df$coenew[i],bty="n",cex=cex_text)
  
}
text(7.3,3.5,"Adjusted age difference\n with 95% CI (days)",cex=1.05*cex_text)

text(0,4,"(a) Age at palliative stage 1",cex=1,font=2)
mtext("Age difference (days)", line=1.3,side=1, cex=cex_lab) 

par(fig=c(0.5,1,0.5,1), new=T)

##################### pathway treatment age mode 2
df=repairmodel # load palliative stage 2 and reparative procedure model results, see how to compute it in pathway age analysis R file. 

plot(NULL,xlim=c(-10,10),ylim=c(0.8,4),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, bty="n",main="")
axis(1, at=seq(-10,10,2),labels=T, cex.axis=1)
lines(rep(0,2),c(0,3.2),lwd=2,lty=2,col=gray(0.4))
y_pos=3:1

for(i in 1:3){
  lines(df[i,3:4],rep(y_pos[i],2),lwd=2,col=gray(0.4))
  points(round(df[i]),round(y_pos[i]),pch=19,col=gray(0.1))
  legend(-11,y_pos[i]+textposition,birth_era[i],bty="n",cex=cex_text)
  legend(2.5,y_pos[i]+3*textposition/4,df$coenew[i],bty="n",cex=cex_text)
  
}
text(7.3,3.5,"Adjusted age difference\n with 95% CI (days)",cex=1.05*cex_text)
text(0,4,"(b) Age at palliative stage 2 or reparative procedure",cex=1,font=2)

mtext("Age difference (days)", line=1.3,side=1, cex=cex_lab) 

##################### days spent at home 
df=nonIPdaysathomemode # load nonIPdaysathome modelling results, see how to compute it in hospital resource unltization R file. 

par(fig=c(0.5,1,0,0.5), new=T)

plot(NULL,xlim=c(-10,10),ylim=c(0.8,4),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, bty="n",main="")
axis(1, at=seq(-10,10,2),labels=T, cex.axis=1)
lines(rep(0,2),c(0,3.2),lwd=2,lty=2,col=gray(0.4))
y_pos=3:1

for(i in 1:3){
  lines(df[i,3:4],rep(y_pos[i],2),lwd=2,col=gray(0.4))
  points(round(df[i]),round(y_pos[i]),pch=19,col=gray(0.1))
  legend(-11,y_pos[i]+textposition,birth_era[i],bty="n",cex=cex_text)
  legend(2.5,y_pos[i]+0.75*textposition,df$coenew[i],bty="n",cex=cex_text)
  
}
text(7.3,3.5,"Adjusted difference\n with 95% CI (days)",cex=1.05*cex_text)
text(0,4,"(d) Days at home before year 1",cex=1,font=2)
mtext("Difference in days at home", line=1.3,side=1, cex=cex_lab) 

#####################infant mortality
df=deathmodel # load infant mortality model results, see how to compute it in infant mortality analysis R file. 
par(fig=c(0,0.5,0,0.5), new=T)

plot(NULL,xlim=c(0,3),ylim=c(0.8,4),pch=19, cex=0.5,col=, yaxt='n',
     xaxt='n', xlab='', ylab='',new=T, bty="n",main="")
axis(1, at=c(0.5,1,1.5,2,2.5),labels=T, cex.axis=1)
lines(rep(1,2),c(0,3.2),lwd=2,lty=2,col=gray(0.4))
y_pos=3:1

for(i in 1:3){
  lines(df[i,3:4],rep(y_pos[i],2),lwd=2,col=gray(0.4))
  points((df[i]),(y_pos[i]),pch=19,col=gray(0.1))
  legend(-0.25,y_pos[i]+textposition,birth_era[i],bty="n",cex=cex_text)
  legend(1.5,y_pos[i]+textposition,df$coenew[i],bty="n",cex=cex_text)
  
}
text(2.2,3.5,"Adjusted OR with 95% CI",cex=1.05*cex_text)
text(1.5,4,"(c) Infant mortality",cex=1,font=2)
mtext("Odds Ratio (OR)", line=1.3,side=1, cex=cex_lab) 

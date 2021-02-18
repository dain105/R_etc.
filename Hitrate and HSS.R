#input
data_dir<-"D:/apcc/2020/기타/2017~2020.07/한반도 예측결과 Lead1~3.xlsx"
out_dir<-"D:/apcc/2020/기타/2017~2020.07/"#out directory for save the results(data and images)

#function
hithss<-function(obs,pred){
  out<-list()
  #obs+ pred+
  ANAN<-ifelse((obs=="+")&(pred=="+"),1,ifelse((obs=="+")&(pred=="0+"),0.5,0))
  #obs0 pred0
  NNNN<-ifelse((obs=="0")&(pred=="0"),1,ifelse((obs=="0")&(pred=="0+"),0.5,ifelse((obs=="0")&(pred=="0-"),0.5,0)))
  #obs- pred-
  BNBN<-ifelse((obs=="-")&(pred=="-"),1,ifelse((obs=="-")&(pred=="0-"),0.5,0))
  
  
  # obs+ pred0
  ANNN<-ifelse((obs=="+")&(pred=="0"),1,ifelse((obs=="+")&(pred=="0+"),0.5,ifelse((obs=="+")&(pred=="0-"),0.5,0)))
  # obs+ pred-
  ANBN<-ifelse((obs=="+")&(pred=="-"),1,ifelse((obs=="+")&(pred=="0-"),0.5,0))
  # obs0 pred+
  NNAN<-ifelse((obs=="0")&(pred=="+"),1,ifelse((obs=="0")&(pred=="0+"),0.5,0))
  NNBN<-ifelse((obs=="0")&(pred=="-"),1,ifelse((obs=="0")&(pred=="0-"),0.5,0))
  # obs- pred+
  BNAN<-ifelse((obs=="-")&(pred=="+"),1,ifelse((obs=="-")&(pred=="0+"),0.5,0))
  # obs- pred0
  BNNN<-ifelse((obs=="-")&(pred=="0"),1,ifelse((obs=="-")&(pred=="0-"),0.5,ifelse((obs=="-")&(pred=="0+"),0.5,0)))
  
  #calculate Hit rate
  Hitrate<-(sum(ANAN)+sum(NNNN)+sum(BNBN))/length(obs)
  
  #calculate HSS
  pfAN<-(sum(ANAN)+sum(NNAN)+sum(BNAN))/length(obs)
  pfNN<-(sum(ANNN)+sum(NNNN)+sum(BNNN))/length(obs)
  pfBN<-(sum(ANBN)+sum(NNBN)+sum(BNBN))/length(obs)
  
  poAN<-(sum(ANAN)+sum(ANNN)+sum(ANBN))/length(obs)
  poNN<-(sum(NNAN)+sum(NNNN)+sum(NNBN))/length(obs)
  poBN<-(sum(BNAN)+sum(BNNN)+sum(BNBN))/length(obs)
  
  pAN<-pfAN*poAN
  pNN<-pfNN*poNN
  pBN<-pfBN*poBN
  hss<-((Hitrate-(pAN+pNN+pBN))/(1-(pAN+pNN+pBN)))*100

  # value return
  out$hitrate<-Hitrate
  out$hss<-hss
  return(out)
}

#make matrix 6*6 maxrix
mat<-matrix(NA,6,ncol=6)
#colum name of mat
colnames(mat)<-c("HR_KMA","hss_KMA","HR_MME","hss_MME","HR_APCC","hss_APCC")
#row names of mat
rownames(mat)<-c("temp Lead1","temp Lead2","temp Lead3","prec Lead1","prec Lead2","prec Lead3")

library(openxlsx)
# i=sheet number 
# 1=1mont temp, 2=2month temp 3=3month temp, 4=1month prec, 5=2month prec, 6=3month prec
for(i in 1:6){ 
  #import data
  data<-read.xlsx(data_dir,sheet=i)
  #calculat KMA
  kma<-hithss(data$OBS,data$KMA)
  mat[i,1]<-kma$hitrate
  mat[i,2]<-kma$hss
  #calculat MME
  mme<-hithss(data$OBS,data$MME)
  mat[i,3]<-mme$hitrate
  mat[i,4]<-mme$hss
  #calculat APCC
  apcc<-hithss(data$OBS,data$APCC)
  mat[i,5]<-apcc$hitrate
  mat[i,6]<-apcc$hss
}
#save mat in out dir
write.csv(mat,paste0(out_dir,"mat.csv"))


######################################################################################plot######################################################################################################

#change columname and rowname for plot
colnames(mat)<-c("KMA","KMA","MME","MME","APCC","APCC")
# rownames(mat)<-c("Lead1","Lead2","Lead3","Lead1","Lead2","Lead3")
#save image
png(paste0(out_dir,"hrtemp.png"), width=500)
hrtemp<-mat[1:3,c(1,3,5)] #temp Hit rate
bp<-barplot(hrtemp,beside=TRUE,col=c(2,3,4),ylim=c(0,1),main="Temp Hit Rate",cex.main=2,cex.names=2,cex.axis=1.5)
legend("topleft",legend=c("1month","2month","3month"),fill=c(2,3,4),cex=1.5)
text(x=bp,y=hrtemp+0.04,labels=round(hrtemp,2),xpd=T,cex=1.2)
dev.off()

png(paste0(out_dir,"hSStemp.png"), width=500)
hSStemp<-mat[1:3,c(2,4,6)]
bp<-barplot(hSStemp,beside=T,col=c(2,3,4),ylim=c(min(hSStemp)-5,max(hSStemp)+10),main="Temp HSS",cex.main=2,cex.names=2,cex.axis=1.5)
legend('topleft',legend=c("1month","2month","3month"),fill=c(2,3,4),cex=1.5)
text(x=bp,y=hSStemp+2,labels=round(hSStemp,2),xpd=T,cex=1.2)
dev.off()

png(paste0(out_dir,"hrprec.png"), width=500)
hrprec<-mat[4:6,c(1,3,5)]
bp<-barplot(hrprec,beside=T,col=c(2,3,4),ylim=c(0,1),main="Prec Hit Rate",cex.main=2,cex.names=2,cex.axis=1.5)
legend('topleft',legend=c("KMA","MME","APCC"),fill=c(2,3,4),cex=1.5)
text(x=bp,y=hrprec+0.04,labels=round(hrprec,2),xpd=T,cex=1.2)
dev.off()

png(paste0(out_dir,"/hSSprec.png"), width=500)
hSSprec<-mat[4:6,c(2,4,6)]
bp<-barplot(hSSprec,ylim=c(min(hSSprec)-5,max(hSSprec)+10),beside=T,col=c(2,3,4),main="Prec HSS at 1month Lead",cex.main=2,cex.names=2,cex.axis=1.5)
legend('topleft',legend=c("KMA","MME","APCC"),fill=c(2,3,4),cex=1.5)
text(x=bp,y=hSSprec+1.5,labels=round(hSSprec,2),xpd=T,cex=1.2)
dev.off()


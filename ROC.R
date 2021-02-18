library(openxlsx)
pred<-"D:/apcc/2020/기타/prob_data.xlsx"
obs<-"D:/apcc/2020/기타/2017~2020.08/한반도 예측결과 Lead1~3.xlsx"
#make matrix 
roc.mat<-matrix(NA,6,ncol=5)
colnames(roc.mat)<-c("var","Lead","B","N","A")
roc.mat[,1]<-c("temp","temp","temp","prec","prec","prec")
roc.mat[,2]<-rep(1:3,2)

for(lead in 1:3){
  for(var in c("temp", "prec")){
    for(BNA in c("+","0","-")){
      BNA1=ifelse(BNA=="+","A",ifelse(BNA=="0","N","B"))
      
      #import data
      sheet<-ifelse(var=="temp",1,2) #if temp, excel sheet=1 if prec excel sheet 2.
      data<-read.xlsx(pred,sheet)
      sheet<-ifelse(var=="temp",1,4)#if temp, excel sheet=1 if prec excel sheet 4.
      obs<-read.xlsx(obs,sheet=sheet)[,c(1,2,6)]
      #colum names to import from data
      colname<-c(paste0("lead",lead,"B"),paste0("lead",lead,"N"),paste0("lead",lead,"A"))
      
      #merging obs and pred  
      lead.data<-data.frame(obs[lead:dim(data)[1],],data[1:(dim(data)[1]-lead+1),c(colname)])
      #make response for roc
      lead.data$response<-ifelse(lead.data$OBS==BNA,1,0)
      
      data<-data.frame(lead.data[,which(colnames(lead.data)==paste0("lead",lead,BNA1))],lead.data$response)
      colnames(data)<-c("prob","response")
      
      p<-unique(data[,1]) 
      #sort unique fcst prob values from largest to smallest
      p<-rev(sort(p))
 
      library(pROC)
      rr<-roc(data$response,data$prob,width=300,height=300)
      rr
      png(paste0("D:/apcc/2020/기타/roc/ROC_lead",lead,"_",var,BNA1,".png"),width=300,height=300)
      plot.roc(rr,col="red",print.auc=T,max.auc.polygon.density = T,auc.polygon = T,auc.polygon.col = "gray90")
      dev.off()
      
      roc.mat[which(roc.mat[,1]==var&roc.mat[,2]==lead),which(colnames(roc.mat)==BNA1)]<-sprintf("%.3f",rr$auc)
      rr
    }
  }  
}
# write.csv(roc.mat,"D:/apcc/2020/기타/roc/roc.mat.csv")

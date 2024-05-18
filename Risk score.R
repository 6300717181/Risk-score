#install.packages('survival')
library(survival)                                                
rt=read.table("survival.txt",header=T,sep="\t",check.names=F,row.names=1)    
rt$OS.time=rt$OS.time/365

#COX
multiCox=coxph(Surv(OS.time, OS) ~ ., data = rt)
multiCox=step(multiCox,direction = "both")
multiCoxSum=summary(multiCox)

#Risk score
riskScore=predict(multiCox,type="risk",newdata=rt)
coxGene=rownames(multiCoxSum$coefficients)
coxGene=gsub("`","",coxGene)
outCol=c("OS.time","OS",coxGene)
risk=as.vector(ifelse(riskScore>median(riskScore),"high","low"))
write.table(cbind(id=rownames(cbind(rt[,outCol],riskScore,risk)),cbind(rt[,outCol],riskScore,risk)),
    file="risk.txt",
    sep="\t",
    quote=F,
    row.names=F)


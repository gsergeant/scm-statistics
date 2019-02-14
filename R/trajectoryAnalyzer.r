
summ=data.frame(t(sapply( unique(tracks$track_id),function(id) {
hlp=subset(tracks,track_id==id)
ch=try(sum(sqrt(rowSums(apply(hlp[chull(hlp$norm_x,hlp$norm_y),c("norm_x","norm_y")],2,diff)^2))))
if (class(ch)=="try-error") ch=0
return(c(unique(hlp$cellType_treatment),id,mean(hlp$delta_z,na.rm=TRUE),sum(hlp$delta_z,na.rm=TRUE),ch))
}
)))


names(summ)=c("cellType_treatment","track_id","delta_z","total","ch_peri")
summ[,1]=as.factor(summ[,1])
levels(summ[,1])=levels(tracks$cellType_treatment)


summ$cellType=factor(sapply(strsplit(as.character(summ$cellType_treatment),split="_"),function(x) x[1]))

summ$treat=factor(sapply(strsplit(as.character(summ$cellType_treatment),split="_"),function(x) x[2]),levels=c("control","10.0 µm" ,"20.0 µm","40.0 µm","60.0 µm","80.0 µm","100.0 µm","cxcl1","cxcl8","fmlp"))


library(colorRamps)
cols=blue2red(7)
plot(density(subset(summ,cellType=="ht1080"&treat==levels(treat)[1])$delta_z,bw=1.5),main="ht1080",xlab="mean delta_z",col=cols[1])
for (i in 2:7)
lines(density(subset(summ,cellType=="ht1080"&treat==levels(treat)[i])$delta_z,bw=1.5),col=cols[i])
legend("topright",col=cols,lty=1,legend=levels(summ$treat)[1:7])

plot(density(subset(summ,cellType=="leukocytes"&treat==levels(treat)[1])$delta_z,bw=.55),main="leukocytes",xlab="mean delta_z",ylim=c(0,.4))
for (i in 2:4)
lines(density(subset(summ,cellType=="leukocytes"&treat==levels(treat)[6+i])$delta_z,bw=.55),col=i)
legend("topright",col=1:4,lty=1,legend=levels(summ$treat)[-(2:7)])



plot(density(log(subset(summ,cellType=="ht1080"&treat==levels(treat)[1])$delta_z+.5),bw=0.25),main="ht1080",xlab="mean delta_z",col=cols[1],ylim=c(0,.8))
for (i in 2:7)
lines(density(log(subset(summ,cellType=="ht1080"&treat==levels(treat)[i])$delta_z+.5),bw=0.25),col=cols[i])
legend("topleft",col=cols,lty=1,legend=levels(summ$treat)[1:7])

plot(density(log(subset(summ,cellType=="leukocytes"&treat==levels(treat)[1])$delta_z+.5),bw=.2),main="leukocytes",xlab="mean delta_z",ylim=c(0,1.5))
for (i in 2:4)
lines(density(log(subset(summ,cellType=="leukocytes"&treat==levels(treat)[6+i])$delta_z+.5),bw=.2),col=i)
legend("topleft",col=1:4,lty=1,legend=levels(summ$treat)[-(2:7)])


###chull

bw=density(subset(summ,cellType=="leukocytes"&treat==levels(treat)[1])$ch_peri)$bw
plot(density(subset(summ,cellType=="leukocytes"&treat==levels(treat)[1])$ch_peri,bw=bw),main="leukocytes",xlab="ch_peri")
for (i in 2:4)
lines(density(subset(summ,cellType=="leukocytes"&treat==levels(treat)[6+i])$ch_peri,bw=bw),col=i)
legend("topright",col=1:4,lty=1,legend=levels(summ$treat)[-(2:7)])


plot(hlp[chull(hlp$norm_x,hlp$norm_y),c("norm_x","norm_y")])
lines(hlp[,c("norm_x","norm_y")],type="l")

######################################

wilcox_sr <- function(argumentColumn) {
return wilcox.test(argumentColumn)
}

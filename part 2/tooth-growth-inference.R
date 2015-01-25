
setwd('~/Source Code/GitHub/Statistical-Inference/part 2')
suppressWarnings(library(ggplot2)); suppressMessages(library(data.table)); 
library(grid);
source('multiplot.R');

# load data and make column names meaningful
dt<-data.table(ToothGrowth)
setnames(dt,c('len','supp','dose'),c('Length','Supplement','Dose'))

# add 'Dosage'and set the join key
dt<-dt[,Dosage:=sapply(as.character(dt$Dose),function(x) as.factor(switch(x,'0.5'='SM','1'='MD','2'='LG')))]
setkey(dt,Supplement,Dosage)

# build an aggregate set
dtc<-dt[,list(avg=mean(Length),var=var(Length),stdv=sd(Length)),by=list(Supplement,Dosage)]
setkey(dtc,Supplement,Dosage)

# some exploratory metadata
str(dt)
summary(dt)

# plot 1 
g1<-ggplot(dt,aes(x=Dosage,y=Length))
g1<-g1+geom_point(aes(color=Supplement),size=5)
# plot 2
g2<-ggplot(dt,aes(x=Supplement,y=Length))
g2<-g2+geom_point(aes(color=Dosage),size=5)
# plot together
g<-multiplot(g1,g2,cols=2)

# SM Dosage vs MD Dosage
t1<-subset(dt,Dosage=='SM')$Length
t2<-subset(dt,Dosage=='MD')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$conf.int[1:2]

# MD Dosage vs LG Dosage
t1<-subset(dt,Dosage=='MD')$Length
t2<-subset(dt,Dosage=='LG')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$conf.int[1:2]

# VC Supplement vs OJ Supplement
t1<-subset(dt,Supplement=='VC')$Length
t2<-subset(dt,Supplement=='OJ')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$p.value
t$conf.int[1:2]

#
t1<-subset(dt,Supplement=='VC' & Dosage=='SM')$Length
t2<-subset(dt,Supplement=='OJ' & Dosage=='SM')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$conf.int[1:2]

#
t1<-subset(dt,Supplement=='VC' & Dosage=='MD')$Length
t2<-subset(dt,Supplement=='OJ' & Dosage=='MD')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$conf.int[1:2]

#
t1<-subset(dt,Supplement=='VC' & Dosage=='LG')$Length
t2<-subset(dt,Supplement=='OJ' & Dosage=='LG')$Length
t<-t.test(t1,t2,paired=FALSE,var.equal=FALSE)
t$conf.int[1:2]




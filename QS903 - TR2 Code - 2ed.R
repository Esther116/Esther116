rm(list = ls())
detach()

#Loading the dataset
library(foreign)
WVS.7<-read.spss("WVS_Cross-National_Wave_7_sav_v1_6.sav",
               to.data.frame=TRUE,max.value.labels=100)
dim(WVS.CN7<-subset(WVS.7,C_COW_NUM=="China"))
attach(WVS.CN7)

#Creating a new dataset regarding to selected variables
Varis<-cbind((WVS.CN7)[,108:113],(WVS.CN7)[,115],(WVS.CN7)[,355],
             (WVS.CN7)[,348:349],(WVS.CN7)[,334],(WVS.CN7)[,337],
             (WVS.CN7)[,340],(WVS.CN7)[,343],(WVS.CN7)[,320],
             (WVS.CN7)[,256:263],(WVS.CN7)[,213:218])
colnames(Varis)<-c("confi_police","confi_courts","confi_government","confi_party",
                   "confi_parliament","confi_civilservice","confi_election",
                   "income","job","job_spouse","edu","edu_spouse","edu_mother",
                   "edu_father","age","DailyNewspaper","TVNews","RadioNews",
                   "Mobile","Email","Internet","Social_Media","Talk")
detach(WVS.CN7)
attach(Varis)

#Pre-processing data
Varis.1<-na.omit(Varis)
Varis.2<-Varis.1
for (i in 1:7) {Varis.2[[i]]<-5-as.numeric(Varis.1[[i]])}
Varis.2[[8]]<-as.numeric(Varis.1[[8]])
for (k in 9:10) {Varis.1[[k]]<-13-as.numeric(Varis.1[[k]])
Varis.2[[k]]<-Varis.1[[k]]
Varis.2[[k]][Varis.1[[k]]>11]<-1}
for (j in 11:14) {Varis.2[[j]]<-as.numeric(Varis.1[[j]])}
Varis.2[[15]]<-as.numeric(as.character(Varis.1[[15]]))
for (l in 15:23) {Varis.2[[l]]<-6-as.numeric(Varis.1[[l]])}
for (m in 24:length(Varis.1)) { Varis.2[[m]]<-as.numeric(Varis.1[[m]])}

#shiyan2
library(corrplot)
Varis.2cor<-cor(Varis.2[1:length(Varis.1)])
dev.new()
corrplot(corr=Varis.2cor, method = "shade",shade.col = NA, tl.col ="black", tl.srt = 45, order = "AOE")








#Correlation Matrix



#jiuming
detach(Varis)
attach(WVS.CN7)
shiyan<-cbind(WVS.CN7)[233:250]
colnames(shiyan)<-c("avoiding fare","stealing","cheat on taxes","bribe","homosexuality","prostitution",
                    "abortion","divorce","premarital_sex","suicide","euthanasia","beating wife",
                    "beating children","violence against other people","terrorism","having casual sex",
                    "political violence","death penalty")
shiyan<-na.omit(shiyan)
shiyan1<-shiyan
for (ii in 1:length(shiyan)) {
  shiyan1[[ii]]<-as.numeric(shiyan[[ii]])
}
library(corrplot)
shiyan_cor<-cor(shiyan1)
dev.new()
corrplot(corr=shiyan_cor, method = "shade",shade.col = NA, tl.col ="black", tl.srt = 45, order = "AOE")
round(shiyan_cor,2)

#factor analysis
library(psych)
library(FactoMineR)
library(GPArotation)
fa.parallel(shiyan1)

f1<-factanal(shiyan1,factors=4,rotation="none")
f2<-factanal(shiyan1,factors=4,rotation="none")


#From 4ed putting in bins
#Omit 2 (age, income)
Varis.4<-Varis.3[1:14]
f7<-factanal(Varis.4,factors = 3,rotation = "none")
f8<-factanal(Varis.4,factors = 3,rotation = "varimax")
f9<-factanal(Varis.4,factors = 3,rotation = "oblimin")

#Comparing models
print(f7,digits=2,cutoff=0,sort=T)
print(f8,digits=2,cutoff=0,sort=T)
print(f9,digits=2,cutoff=0,sort=T)
round(rbind(f7$uniqueness,f8$uniqueness,f9$uniqueness),2)
round(cbind(f7$loadings,f8$loadings,f9$loadings),2)
#~Uniqueness remain unchanged among models
#~f9 better because its factors have more distinct loading to one another
#~ has higher higher ss loadings, proportion var and cumulative var

print(f6,digits=2,cutoff=0,sort=T)
print(f9,digits=2,cutoff=0,sort=T)
round(f9$uniqueness,2)
round(f6$uniqueness,2)
round(f9$loadings,2)
round(f6$loadings,2)

Varis.5<-cbind(Varis.3[1:14],Varis.3[16])
f7.1<-factanal(Varis.5,factors = 3,rotation = "none")
f8.1<-factanal(Varis.5,factors = 3,rotation = "varimax")
f9.1<-factanal(Varis.5,factors = 3,rotation = "oblimin")

#Rotation
f2<-factanal(Varis.2.2,factors = 3,rotation = "varimax")
f3<-factanal(Varis.2.2,factors = 3,rotation = "oblimin")
#~ f2 is better because it has higher ss loadings, proportion var and cumulative var 

#Rotation
f2<-factanal(Varis.2.2,factors = 3,rotation = "varimax")
f3<-factanal(Varis.2.2,factors = 3,rotation = "oblimin")
#~ f2 is better because it has higher ss loadings, proportion var and cumulative var 
#~ 3 factors model was chosen for the analysis because it has the suitable loading without extremely high results
# and have the reasonable uniqueness
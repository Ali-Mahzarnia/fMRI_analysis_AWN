
library(readxl)
library(dplyr)
library(magrittr) 

path_master='/Users/ali/Desktop/Feb23/fmri/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(DWI,Genotype,Weight, Sex, Diet, Age_Months, CIVMID, BadeaID, ARunno)#subselect

path_connec="/Users/ali/Desktop/Feb23/fmri/time_ser/"
file_list=list.files(path_connec)
ts_namesorig = file_list[grepl("^ts_.*", file_list)]
ts_names = gsub('ts_','',ts_namesorig) 
ts_names = gsub('.csv','',ts_names)
which(ts_names [33] == datatemp$ARunno )

index = match(ts_names, datatemp$ARunno  )
data_temp_fmri = datatemp [index, ]

temp = read.csv(paste0(path_connec,ts_namesorig[1]), header = F)
X = array(NaN, c(dim(temp)[1], length(ts_names), 299))

for (i in 1:length(ts_names)) {
  temp = read.csv(paste0(path_connec,ts_namesorig[i]), header = F)
  temp = as.matrix(temp)
  if(ts_names[i]==data_temp_fmri$ARunno[i] ) {X[,i,] = temp [, 1:299]}
}

Y = data_temp_fmri$Age_Months
Y =as.matrix(t(Y))
dim(Y)
sd(Y)

n = length(ts_names)
trainIndex=sample(1:n, size = round(0.8*n), replace=FALSE)
Ytrain=Y[, trainIndex, drop = FALSE ]
Ytest=Y[, -trainIndex, drop = FALSE ]
Xtrain=X[,trainIndex,, drop=FALSE]
Xtest=X[,-trainIndex,, drop=FALSE]

tt=(1:299)/299

par(mfrow=c(3,3))
for(j in 1:9){
  plot(tt,Xtrain[j,1,],type='l', ylim=c(-30,30), main=paste0("j=",j))
  for(k in 2:10)lines(tt, Xtrain[j,k,])
}

library(MFSGrp) 

# run
m=17 #basisino
p= dim(temp)[1]
part=rep(m,p) # partition

# green: true beta  (only beta5, beta8, beta11 are the nonzero functions)
# black: estimated betas

# lasso 
# in order to see all figures after the run, use the "previous plot" arrow on Rstudio
results=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,
               Ypred=Ytest, Penalty = "glasso" , bspline=TRUE, sixplotnum="max" , 
               lambdaderivative = 0)
sqrt(results$MSEpredict)  # test Root MSE
sum(results$coef==0)/m    # number of zero functional coefficients
results$lambda # the regularized lambda
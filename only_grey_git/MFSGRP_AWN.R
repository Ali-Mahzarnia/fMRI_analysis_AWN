
library(readxl)
library(dplyr)
library(magrittr) 

path_master='/Users/ali/Desktop/Jul23/fmri_jayvik/codes/only_gray/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(DWI,Genotype,Weight, Sex, Diet, Age_Months, CIVMID, BadeaID, ARunno)#subselect

path_connec="/Users/ali/Desktop/Jul23/fmri_jayvik/codes/time_ser/"
file_list=list.files(path_connec)
ts_namesorig = file_list[grepl("^ts_.*", file_list)]
ts_names = gsub('ts_','',ts_namesorig) 
ts_names = gsub('.csv','',ts_names)
which(ts_names [33] == datatemp$ARunno )

nt=160
tstart=30
tend= tstart + nt-1


index = match(ts_names, datatemp$ARunno  )
ts_names = ts_names[!is.na(index)]
index = na.omit(index)
data_temp_fmri = datatemp [index, ]

temp = read.csv(paste0(path_connec,ts_namesorig[1]), header = F)
X = array(NaN, c(dim(temp)[1], length(ts_names), nt))

for (i in 1:length(ts_names)) {
  temp = read.csv(paste0(path_connec,ts_namesorig[i]), header = F)
  temp = as.matrix(temp)
  if(ts_names[i]==data_temp_fmri$ARunno[i] ) {X[,i,] = temp [, tstart:tend]}
}

MWM_data_path= '/Users/ali/Desktop/Feb23/fmri/Dist_table_Anna_NEW.csv'
MWM_data = read.csv(MWM_data_path)


index_match = match( data_temp_fmri$BadeaID, MWM_data$AnimalID)

data_temp_fmri$BadeaID[!is.na(index_match )]


data_temp_fmri = data_temp_fmri[!is.na(index_match ), ]
MWM_data =  MWM_data[index_match,]
MWM_data = na.omit(MWM_data)
X = X[, !is.na(index_match ) ,,drop=T]

##### grey matter only :

atlas_path= '/Users/ali/Desktop/Jul23/fmri_jayvik/codes/only_gray/new_atlas.csv'
new_atlas = read.csv(atlas_path)

sum(grepl("Ventricle", new_atlas$Structure, fixed = TRUE)) 
sum(new_atlas$Subdivisions_7 =="8_CSF")

grey_index = !new_atlas$Level_4 == "white_matter" & !grepl("Ventricle", new_atlas$Structure, fixed = TRUE) & !new_atlas$Subdivisions_7 =="8_CSF"
sum(grey_index)

new_atlas = new_atlas[grey_index, ]

X = X[grey_index, , , drop=T]






Y = MWM_data$Probe_D8_T1
Y =as.matrix(t(Y))
dim(Y)
sd(Y)

n = length(Y)
trainIndex=sample(1:n, size = round(0.9*n), replace=FALSE)
Ytrain=Y[, trainIndex, drop = FALSE ]
Ytest=Y[, -trainIndex, drop = FALSE ]
sd(Ytest)
Xtrain=X[,trainIndex,, drop=FALSE]
Xtest=X[,-trainIndex,, drop=FALSE]

tt=(1:nt)/nt

par(mfrow=c(3,3))
for(j in 1:9){
  plot(tt,Xtrain[j,1,],type='l', ylim=c(-5,5), main=paste0("j=",j))
  for(k in 2:10)lines(tt, Xtrain[j,k,])
}




library(MFSGrp) 

# run
m=7 #basisino
p= dim(temp)[1]
part=rep(m,p) # partition

# green: true beta  (only beta5, beta8, beta11 are the nonzero functions)
# black: estimated betas

# lasso 
# in order to see all figures after the run, use the "previous plot" arrow on Rstudio
results=MFSGrp(Ytrain,Xtrain,basisno=m,tt, part=part,Xpred=Xtest,
               Ypred=Ytest, Penalty = "gelast", sixplotnum="max",forcezero =F, bspline = F,
               eps = 1e-08, 
               maxit = 3e+08)
sqrt(results$MSEpredict)  # test Root MSE
sum(results$coef==0)/m    # number of zero functional coefficients
#results$lambda # the regularized lambda

View(new_atlas) # for interpretations



# TIME SERIES OF RESUTS:
colors= colorspace::qualitative_hcl(7, palette = "Dark 3")
par(mfrow=c(2,12))
for(j in c(18,55,70,88,94,99,100,102,105,118,120,135,147,186,212,214,222,224,228,230,237,247,252)){
  plot(tt,Xtrain[j,1,],type='l', ylim=c(-10,10), main=paste0("j=",j) , col = colors)
  for(k in 2:9)lines(tt, Xtrain[j,k,],  col = colors[k])
}



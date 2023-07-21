
library(readxl)
library(dplyr)
library(magrittr) 

path_master='/Users/ali/Desktop/Feb23/fmri/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(DWI,Genotype,Weight, Sex, Diet, Age_Months, CIVMID, BadeaID)#subselect
datatemp$CIVMID=gsub(":.*","",datatemp$CIVMID)
datatemp$CIVMID=gsub("_", "-", datatemp$CIVMID)
temp_badeaID= datatemp$CIVMID
temp_badeaID = gsub("-", "", temp_badeaID)
tempindex7 = nchar(temp_badeaID) ==7
tempindex8 = nchar(temp_badeaID) ==8
new_badeaID =datatemp$CIVMID 
new_badeaID[tempindex7] = gsub("-", "0", new_badeaID[tempindex7])
new_badeaID[tempindex8] = gsub("-", "", new_badeaID[tempindex8])
datatemp$newBadeaID=new_badeaID


path_connec="/Users/ali/Desktop/Feb23/fmri/time_ser/"
file_list=list.files(path_connec)
ts_names = file_list[grepl("^ts_.*", file_list)]
ts_names = gsub('ts_A','',ts_names) 
ts_names = gsub('.csv','',ts_names)
which(ts_names [33] == datatemp$newBadeaID )

index = match(ts_names, datatemp$newBadeaID  )
data_temp_fmri = datatemp [index, ]

age = data_temp_fmri$Age_Months
age[as.numeric(data_temp_fmri$Age_Months) < median(data_temp_fmri$Age_Months)]= 1
age[data_temp_fmri$Age_Months >= median(data_temp_fmri$Age_Months)] = 2
data_temp_fmri$age_cat = age

table(data_temp_fmri$Genotype,data_temp_fmri$Diet )
table(data_temp_fmri$Genotype, data_temp_fmri$age_cat, data_temp_fmri$Diet)
table(data_temp_fmri$Sex)
table(data_temp_fmri$Diet)
table(data_temp_fmri$Diet)
table(data_temp_fmri$age_cat)
library("xlsx")
write.xlsx( data_temp_fmri , '/Users/ali/Desktop/Feb23/fmri/fmri_sheet.xlsx'  )

cardiac_path =  '/Users/ali/Desktop/Feb23/fmri/Cardiac_LV_results_01132023.xlsx'
data_cardiac=read_xlsx(cardiac_path)
index_cardiac = match( data_cardiac$ID, gsub("_", "-", data_temp_fmri$BadeaID) )
data_cardiac$ID[is.na(index_cardiac)]
data_temp_fmri = datatemp [index, ]
